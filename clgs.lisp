;;;; Golfscript interpreter
(in-package :clgs)

;;; Stack & related functions
(declaim (list *stack*))
(defvar *stack* ()
  "Main golfscript stack.")

(defun stack-push (object &rest more-objects)
  "Push all non-null args onto the golfscript stack."
  (unless (null object)
    (push object *stack*))
  (unless (endp more-objects)
    (apply #'stack-push more-objects)))

(defvar *stack-mark* ()
  "The [ function uses this to mark stack size, and ] slices back to it")

(defun stack-pop  ()
  "Pop the top item off the golfscript stack and return it."
  (loop for cons on *stack-mark*
    when (< (car cons) (length *stack*))
      return ()
    do (decf (car cons)))
  (pop *stack*))

(defun stack-peek (&optional (depth 1))
  "Return the first DEPTH elements from the top of the golfscript
  stack, without modifiying it."
  (subseq *stack* 0 depth))

(defun stack-elt (index)
  "Return the element at position INDEX in the stack"
  (elt *stack* index))

(defun set-stack (stack-values)
  "Remove all current stack values and set stack to the values in STACK-VALUES"
  (setf *stack* (the list stack-values)))

;;; Variable table & related functions
(defvar *variable-table* (make-hash-table)
  "Holds variable definitions, including functions.
  Indexed by symbols")

(defun add-to-var-table (name value table)
  "Add VALUE to TABLE under key NAME"
  (setf (gethash (the symbol name) table)
        (the function value)))

(defun get-from-var-table (name table)
  "Retrieve the entry in TABLE stored under NAME. Secondary return value
  indicates whether NAME was found"
  (the (or function null) (gethash (the symbol name) table)))

(defun clear-var-table (table)
  "Removes all values from TABLE"
  (clrhash table))

(defun reset-var-table ()
  "Restore variable table to builtins only"
  (clear-var-table *variable-table*)
  (loop for var being the hash-keys in *builtins* using (hash-value value) do
        (add-to-var-table var value *variable-table*)))

;;; Parsing
(defvar *tokenize-cache* (make-hash-table :test #'equal)
  "Holds tokenized strings to improve performance when executing a block
  many times")

(defun tokenize (gs-code-string)
  "Return a list of string tokens from golfscript source."
  (or (gethash gs-code-string *tokenize-cache*)
      (setf
        (gethash gs-code-string *tokenize-cache*)
        (let 
          ((tokens 
             (cl-ppcre:all-matches-as-strings
               ;; TODO: Doesn't tokenize strings with escaped quotes correctly
               "(?x)                    # Enable optional whitespace & comments
                [a-zA-Z_][a-zA-Z0-9_]*| # variable name
                '(?:\\.|[^'])*'?|       # single quoted string
                \"(?:\\.|[^\"])*\"?|    # double quoted string
                -?[0-9]+|               # integer
                [#][^\\n\\r]*|          # comment
                [ ]+|                   # whitespace
                [^ ]                    # single character token"
               ;; whitespace is kept here, as otherwise the following happens
               ;; when tokenizing blocks, for example {1 2+}:
               ;; regex splits it into ("{" "1" "2" "+" "}")
               ;; the following DO loop concatenates them into one block token:
               ;; "{12+}", and the semantics of the block are changed.
               ;; So, instead we keep whitespace here, and then filter it out
               ;; after processing blocks
               gs-code-string)))
          (do ((processed-tokens 
                 ()
                 (cons
                   (if (string= (car tokens) "{")
                     (reduce (lambda (a b) (concatenate 'string a b))
                             (loop for token = (pop tokens)
                                   with {}-count = 0
                                   collecting token 
                                   when (string= token "{") do (incf {}-count)
                                   when (string= token "}") do (decf {}-count)
                                   until (or (zerop {}-count) (endp tokens))
                                   finally (unless (zerop {}-count) 
                                             (error "Unmatched { in program ~S"
                                                    gs-code-string))))
                     (pop tokens))
                   processed-tokens)))
            ((not tokens) 
             (if (find "}" processed-tokens :test #'string=)
               (error "Unmatched } in program ~S" gs-code-string)
               (remove-if (lambda (token)
                            (every (lambda (char) (char= char #\Space)) token))
                          (nreverse processed-tokens)))))))))

(defun read-gs-literal (token-string)
  "Read a literal string or integer token into a golfscript type.
  Note that [ and ] do not delimit array literals; they are defined
  as built-in functions."
  (declare (type string token-string))
  (case (char token-string 0)
    (#\' (make-gs-string-from
           (map 'simple-vector #'gs-integer<-char
                (cl-ppcre:regex-replace-all 
                  "([^\\\\]|^)\\\\" (string-trim "'" token-string) "\\1"))))
    (#\{ (make-gs-block-from
           (map 'simple-vector #'gs-integer<-char
                (subseq token-string 1 (1- (length token-string))))))
    ;; TODO: Doesn't handle some escape sequences i.e. \n
    (#\" (make-gs-string-from
           (map 'simple-vector #'gs-integer<-char
                (string-trim "\"" token-string))))
    (otherwise (make-gs-integer-from 
                 (read-from-string token-string)))))

(defun gs-literal-p (token-string)
  (let ((first-char (char token-string 0)))
    (or (member first-char
                '(#\' #\" #\{))
        (and (char-equal first-char #\-)
             (> (length token-string) 1))
        (digit-char-p first-char))))

(defun gs-comment-p (token-string)
  (char-equal (char token-string 0) #\#))

(defun truth-value (gs-object)
  "Return GS-OBJECT if GS-OBJECT evaluates as a true
  boolean in golfscript, or nil otherwise"
  (not (member (gs-var-value gs-object) '(#() 0)
               :test #'equalp)))

(defun execute-gs-program (gs-code-string &optional stack-values)
  "Execute GS-CODE-STRING as golfscript code, optionally providing
  starting stack values. Print stack on completion"
  (set-stack stack-values)
  (setf *stack-mark* ())
  (reset-var-table)
  (execute-gs-string gs-code-string)
  (pretty-print-stack *stack*))

(defun pretty-print-stack (stack)
  "Print each item on the stack in its GS-REPR form"
  (format t "(~{~A~^ ~})~%"
          (mapcar (lambda (gs-object)
                    (map 'string #'char<-gs-integer
                         (gs-var-value (gs-repr gs-object))))
                  (reverse stack))))

(defun execute-gs-string (gs-code-string)
  "Execute string or vector of gs-integer char-codes as golfscript
  code. Doesn't reset stack or variable table"
  ;; When a non-string vector is passed, it's a gs-string
  ;; of gs-integer char-codes
  (when (plusp (length gs-code-string))
    (when (and (vectorp gs-code-string)
               (not (stringp gs-code-string)))
      (setf gs-code-string
            (map 'string (lambda (gs-int)
                           (char<-gs-integer gs-int))
                 gs-code-string)))
    (let ((tokens (tokenize gs-code-string)))
      (do ((token (pop tokens) (pop tokens)))
        ((null token))
        (cond
          ((gs-comment-p token))
          ((gs-literal-p token)
           (stack-push (read-gs-literal token)))
          ;; OPTIMIZATION: Performs GETHASH twice 
          ((get-from-var-table (intern token) *variable-table*)
           (call-gs-fun (intern token)))
          ((string= token ":")
           ;; Variables work by adding a closure to the variable table
           ;; which either executes a block or returns a value
           (let ((var-value (stack-elt 0)))
             (add-to-var-table (intern (pop tokens))
                               (if (gs-block-p var-value)
                                 (lambda ()
                                   (execute-gs-string (gs-var-value var-value)))
                                 (lambda () (stack-push var-value)))
                               *variable-table*)))
          (t (error "Unrecognized token ~S" token)))))))

(defun compile-gs-string (gs-code-string)
  "Return a lambda function which executes the golfscript program
  in GS-CODE-STRING. As much work as possible is done at compile-time"
  (let ((lambda-body ())
        (tokens (tokenize gs-code-string)))
    (do ((token (pop tokens) (pop tokens)))
      ((not token))
      (push
        (cond
          ((gs-literal-p token)
           `(stack-push ,(read-gs-literal token)))
          ((get-from-var-table (intern token) *variable-table*)
           `(call-gs-fun (quote ,(intern token))))
          ((string= token ":")
           (let ((name (intern (pop tokens))))
             ;; Prototype the variable so that later references during
             ;; compilation are recognized as a valid token
             (add-to-var-table name t *variable-table*)
             `(let ((var-value (stack-elt 0)))
                (add-to-var-table (quote ,name)
                                  (if (gs-block-p var-value)
                                    (lambda ()
                                      (execute-gs-string
                                        (gs-var-value var-value)))
                                    (lambda () (stack-push var-value)))
                                  *variable-table*))))
          ((gs-comment-p token))
          (t (error "Unrecognized token ~S" token)))
        lambda-body))
    (eval `(lambda () ,@(reverse lambda-body)))))
