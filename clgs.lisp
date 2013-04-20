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

(defun stack-pop  ()
  "Pop the top item off the golfscript stack and return it."
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

;;; Types
(defstruct (gs-integer
             (:constructor make-gs-integer-from (value))
             (:constructor make-gs-integer))
  (value 0   :type integer))
(defstruct (gs-array
             (:constructor make-gs-array-from   (value))
             (:constructor make-gs-array))
  (value #() :type simple-vector))
(defstruct (gs-string
             (:constructor make-gs-string-from  (value))
             (:constructor make-gs-string)
             (:include gs-array)))
(defstruct (gs-block
             (:constructor make-gs-block-from   (value))
             (:constructor make-gs-block)
             (:include gs-string)))

(defun gs-var-value (gs-var)
  "Return the value contained in a golfscript variable"
  (declare (type (or gs-integer gs-array) gs-var))
  (slot-value gs-var 'value))

(defvar *priority-alist*
  '((gs-block   . 4)
    (gs-string  . 3)
    (gs-array   . 2)
    (gs-integer . 1)
    (null       . 0)))

(defun priority (object)
  "Return coercion priority for OBJECT (higher values take precedence)"
  (cdr (assoc (type-of object) *priority-alist*)))

(defun type-of-priority (priority)
  "Return the type mapped to a particular priority"
  (car (rassoc priority *priority-alist*)))

(defun make-same-type (gs-object value)
  "Return a new gs-object containing VALUE of the same type as GS-OBJECT"
  (etypecase gs-object
    (gs-block   (make-gs-block-from value))
    (gs-string  (make-gs-string-from value))
    (gs-array   (make-gs-array-from value))
    (gs-integer (make-gs-integer-from value))))

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
                [^ ]                    # single character token"
               gs-code-string)))
          (do ((processed-tokens ()
                                 (cons
                                   (if (string= (car tokens) "{")
                                     (reduce (lambda (a b)
                                               (concatenate 'string a b))
                                             (loop for token = (pop tokens)
                                                   with {}-count = 0
                                                   collecting token 
                                                   when (string= token "{")
                                                     do (incf {}-count)
                                                   when (string= token "}")
                                                     do (decf {}-count)
                                                   until (or (zerop {}-count)
                                                             (null tokens))
                                                   finally
                                                   (unless (zerop {}-count) 
                                                     (error "Unmatched { in program ~S"
                                                            gs-code-string))))
                                     (pop tokens))
                                   processed-tokens)))
            ((not tokens) 
             (if (find "}" processed-tokens :test #'string=)
               (error "Unmatched } in program ~S" gs-code-string)
               (nreverse processed-tokens))))))))

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

(defun gs-integer<-char (char)
  (make-gs-integer-from (char-code char)))

(defun char<-gs-integer (gs-int)
  (code-char (gs-var-value gs-int)))

(defun gs-literal-p (token-string)
  (declare (string token-string))
  (let ((first-char (char token-string 0)))
    (or (member first-char
                '(#\' #\" #\{))
        (and (char-equal first-char #\-)
             (> (length token-string) 1))
        (digit-char-p first-char))))

(defun gs-comment-p (token-string)
  (declare (string token-string))
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
                    (map 'string
                         #'char<-gs-integer
                         (gs-var-value
                           (gs-repr gs-object))))
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
          ;; OPTIMIZATION: Loads of duplication here
          ((get-from-var-table (intern token) *variable-table*)
           (call-gs-fun (intern token)))
          ((string= token ":")
           ;; Variables work by adding a closure to the variable table
           ;; which either executes a block or returns a value
           (let ((var-value (stack-elt 0)))
             (add-to-var-table (intern (pop tokens))
                               (if (gs-block-p var-value)
                                 (lambda ()
                                   (execute-gs-string 
                                     (gs-var-value var-value)))
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

(defun coerce-args (args)
  "Given a list of arguments, coerce all to highest priority type"
  (let ((highest-priority-type
          (type-of-priority
            (apply #'max
                   (mapcar #'priority args)))))
    (mapcar (lambda (arg)
              (coerce-gs-object arg highest-priority-type))
            args)))

(defun coerce-gs-object (object type)
  "Return OBJECT coerced to TYPE. Signal error if invalid coercion"
  (cond
    ((typep object type) object) 
    ((null object) nil)
    ;; When coercing from nil, we have reached the bottom of the stack.
    ;; This isn't always an error though; let DEFINE-GS-FUNCTION handle it
    (t
     (let ((value (gs-var-value object)))
       (etypecase object
         (gs-integer
           (ecase type
             (gs-array  (make-gs-array-from  (vector value)))
             (gs-string (make-gs-string-from (map 'simple-vector #'gs-integer<-char
                                                  (write-to-string value))))
             (gs-block  (make-gs-block-from (gs-var-value
                                              (coerce-gs-object object 'gs-string))))))
         (gs-string
           (ecase type
             (gs-block (make-gs-block-from value))))
         (gs-array
           (ecase type
             ;; TODO: This breaks on nested arrays, e.g. [[51 52][53 54]]"55"+
            ;; and non-integer arrays
            (gs-string (make-gs-string-from value))
            (gs-block  (make-gs-block-from 
                         (reduce (lambda (a b)
                                   (concatenate
                                     'simple-vector
                                     a
                                     (vector (gs-integer<-char #\Space))
                                     b))
                                 value
                                 :key (lambda (item)
                                        (gs-var-value
                                          (coerce-gs-object
                                            item
                                            'gs-string)))))))))))))

(defun gs-repr (object)
  "Return the gs-string that returns OBJECT when eval'd in golfscript"
  (let ((value (gs-var-value object)))
    (flet ((surround (start-character value end-character)
             (concatenate 'simple-vector
                      (vector (gs-integer<-char start-character))
                      value
                      (vector (gs-integer<-char end-character)))))
      (make-gs-string-from
        (etypecase object
          (gs-block
            (surround #\{ value #\}))
          (gs-string
            (surround #\' value #\'))
          (gs-array
            (surround #\[
                      (if (zerop (length value))
                        #()
                        (reduce (lambda (a b)
                                (concatenate 'simple-vector
                                             a
                                             (vector (gs-integer<-char #\Space))
                                             (gs-var-value (gs-repr b))))
                              value
                              :initial-value (gs-var-value (gs-repr (elt value 0)))
                              :start 1))
                      #\]))
          (gs-integer
            (map 'simple-vector #'gs-integer<-char
                 (write-to-string value))))))))
