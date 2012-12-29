;;;; Golfscript interpreter

(ql:quickload 'cl-ppcre)

;;; Stack & related functions
(defvar *stack* ()
  "Main golfscript stack.")

(defun stack-push (object &rest more-objects)
  "Push all args onto the golfscript stack."
  (push object *stack*)
  (when more-objects
    (loop for object in more-objects do
          (push object *stack*))))

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
  (setf *stack* stack-values))

;;; Variable table & related functions
(defvar *variable-table* (make-hash-table)
  "Holds variable definitions, including functions.
  Indexed by symbols")

(defun add-to-var-table (name value table)
  "Add VALUE to TABLE under key NAME"
  (setf (gethash name table) value))

(defun get-from-var-table (name table)
  "Retrieve the entry in TABLE stored under NAME. Secondary return value
  indicates whether NAME was found"
  (gethash name table))

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
             (:constructor make-gs-integer (value)))
  (value 0   :type integer))
(defstruct (gs-array
             (:constructor make-gs-array   (value)))
  (value #() :type vector))
(defstruct (gs-string
             (:constructor make-gs-string  (value)))
  (value ""  :type string))
(defstruct (gs-block
             (:constructor make-gs-block   (value)))
  (value ""  :type string))

(defun gs-var-value (gs-var)
  "Return the value contained in a golfscript variable"
  (slot-value gs-var 'value))


(defun priority (object)
  "Return coercion priority for OBJECT (higher values take precedence)"
  (etypecase object
    (gs-integer 1)
    (gs-array   2)
    (gs-string  3)
    (gs-block   4)))

(defun type-of-priority (priority)
  "Return the type mapped to a particular priority"
  (ecase priority
    (1 'gs-integer)
    (2 'gs-array)
    (3 'gs-string)
    (4 'gs-block)))

(defun make-gs-object (object)
  "Return an appropriate golfscript object containing the value in OBJECT.
  Lists get converted into GS-ARRAYs. Strings will always get turned into 
  GS-STRINGs - never blocks"
  (etypecase object
    (integer (make-gs-integer object))
    (string  (make-gs-string  object))  
    (vector  (make-gs-array   object))
    (list    (make-gs-array   (vector object)))))

;;; Parsing

(defun tokenize (gs-code-string)
  "Return a list of string tokens from golfscript source."
  (cl-ppcre:all-matches-as-strings
    ;; TODO: Doesn't tokenize strings with escaped quotes correctly
    ;; ---variable name---------{block}-----'string'----------"string"-------integer-----comment--single character token
    "[a-zA-Z_][a-zA-Z0-9_]*|{(?:\\.|[^'])*}|'(?:\\.|[^'])*'?|\"(?:\\.|[^\"])*\"?|-?[0-9]+|#[^\\n\\r]*|."
    gs-code-string))

(defun read-gs-literal (token-string)
  "Read a literal string or integer token into a golfscript type.
  Note that [ and ] do not delimit array literals; they are defined
  as built-in functions."
  (case (char token-string 0)
    (#\' (make-gs-string
           (cl-ppcre:regex-replace-all 
             "([^\\\\]|^)\\\\" (string-trim "'" token-string) "\\1")))
    (#\{ (make-gs-block
           (string-trim "{}" token-string)))
    ;; TODO: Doesn't handle some escape sequences i.e. \n
    (#\" (make-gs-string
           (string-trim "\"" token-string)))
    (otherwise (make-gs-integer 
                 (read-from-string token-string)))))

(defun gs-literal-p (token-string)
  (declare (type string token-string))
  (let ((first-char (char token-string 0)))
    (or (member first-char
                '(#\' #\" #\{ #\-))
        (digit-char-p first-char))))

(defun gs-comment-p (token-string)
  (char-equal (char token-string 0) #\#))

(defun execute-gs-program (gs-code-string &optional stack-values)
  "Execute GS-CODE-STRING as golfscript code, optionally providing
  starting stack values. Return value of stack on completion"
  (set-stack stack-values)
  (reset-var-table)
  (execute-gs-string gs-code-string)
  ;; TODO: Print stack in a more readable fashion
  (reverse *stack*)) 

(defun execute-gs-string (gs-code-string)
  "Execute string as golfscript code. Doesn't reset stack or variable table"
  (loop for token in (tokenize gs-code-string) do
        (cond
          ((gs-literal-p token)
           (stack-push (read-gs-literal token)))
          ;; OPTIMIZATION: Loads of duplication here
          ((get-from-var-table (intern token) *variable-table*)
           (call-gs-fun (intern token)))
          ((string-equal token " "))
          ((gs-comment-p token))
          (t (error "Unrecognized token ~S" token))))) 

(defun match-arg-type (arg-combinations)
  "Inspect the stack and return the first matching argument combination.
  Signal an error if no combinations match"
  (or (loop for arg-combination in arg-combinations
            when (loop for arg-type    in arg-combination
                       for stack-value in *stack*
                       always (eq arg-type
                                  (type-of stack-value)))
            return arg-combination)
      (error "No matching argument combination in stack for ~S"
             arg-combinations)))

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
  (if (eql type (type-of object))
    object
    (let ((value (gs-var-value object)))
      (etypecase object
        (gs-integer
          (ecase type
            (gs-array  (make-gs-array  (vector value)))
            (gs-string (make-gs-string (write-to-string value)))
            (gs-block  (make-gs-block 
                         (concatenate 'string (write-to-string value) " ")))))
        (gs-array
          (ecase type
            ;; TODO: This breaks on nested arrays, e.g. [[51 52][53 54]]"55"+
            ;; and non-integer arrays
            (gs-string (make-gs-string (map 'string (lambda (x)
                                                      (code-char (gs-var-value x)))
                                            value)))
            (gs-block  (make-gs-block 
                         ;; Coerce all elements of the array to
                         ;; gs-string, and join with " "
                         (reduce (lambda (a b)
                                   (concatenate 'string a " " b)) 
                                 (map 'list (lambda (x)
                                              (slot-value (coerce-gs-object x 'gs-string)
                                                          'value))
                                      value)
                           :from-end t :initial-value "")))))
        (gs-string
          (ecase type
            (gs-block (make-gs-block (concatenate 'string value " ")))))))))

(load "builtins.lisp")
