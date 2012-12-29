;;;; Golfscript interpreter

(ql:quickload 'cl-ppcre)

;;; Stack & related functions
(defvar *stack* ()
  "Main golfscript stack.")

(defun stack-push (object)
  "Push OBJECT onto the golfscript stack."
  (push object *stack*))

(defun stack-pop  ()
  "Pop the top item off the golfscript stack and return it."
  (pop *stack*))

(defun stack-peek (&optional (depth 1))
  "Return the first DEPTH elements from the top of the golfscript
  stack, without modifiying it."
  (subseq *stack* 0 depth))

;;; Parsing

(defun tokenize (gs-code-string)
  "Return a list of string tokens from golfscript source."
  (cl-ppcre:all-matches-as-strings
    ;; Regex taken from golfscript.rb source
    ;; ---variable name---------{block}-----'string'----------"string"-------integer-----comment--single character token
    "[a-zA-Z_][a-zA-Z0-9_]*|{(?:\\.|[^'])*}|'(?:\\.|[^'])*'?|\"(?:\\.|[^\"])*\"?|-?[0-9]+|#[^\\n\\r]*|."
    gs-code-string))

(defun read-gs-literal (token-string)
  "Read a literal string or integer token into a golfscript type.
  Note that [ and ] do not delimit array literals; they are defined
  as built-in functions."
  (case (char token-string 0)
    (#\' (cl-ppcre:regex-replace-all 
           "([^\\\\]|^)\\\\" (string-trim "'" token-string) "\\1"))
    (#\{ (make-gs-block
           (string-trim "{}" token-string)))
    ;; TODO: Doesn't handle some escape sequences i.e. \n
    (#\" (make-gs-string
           (string-trim "\"" token-string)))
    ;; Normal Lisp reader will do for integers
    (otherwise (make-gs-integer 
                 (read-from-string token-string)))))

;;; Structs used for the 4 types
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

(defvar *variable-table* (make-hash-table)
  "Holds variable definitions, including functions.
  Indexed by symbols")

(defun priority (type)
  "Return coercion priority for a type (higher values take precedence)"
  (etypecase type
    (gs-integer 1)
    (gs-array   2)
    (gs-string  3)
    (gs-block   4)))

(defun type-of-priority (priority)
  "Return type mapped to a particular priority"
  (ecase priority
    (1 'gs-integer)
    (2 'gs-array)
    (3 'gs-string)
    (4 'gs-block)))

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
              (coerce-gs-type arg highest-priority-type))
            args)))

(defun coerce-gs-object (object type)
  "Return OBJECT coerced to TYPE. Signal error if invalid coercion"
  (if (eql type (type-of object))
    object
    (let ((value (slot-value object 'value)))
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
                                                      (code-char (slot-value x 'value)))
                                            value)))
            (gs-block  (make-gs-block 
                         ;; Coerce all elements of the array to
                         ;; gs-string, and join with " "
                         (reduce (lambda (a b)
                                   (concatenate 'string a " " b)) 
                                 (map 'list (lambda (x)
                                              (slot-value (coerce-gs-type x 'gs-string)
                                                          'value))
                                      value)
                           :from-end t :initial-value "")))))
        (gs-string
          (ecase type
            (gs-block (make-gs-block (concatenate 'string value " ")))))))))

(defmacro define-gs-function ((name &optional (coerce-n 0)) &body arg-cases)
  "Define a new function and insert it into the variable table.
  Each case defines a different function to perform depending
  on the types of the arguments."
  ;; Check for invalid types in ARG-CASES
  (dolist (arg-case arg-cases)
    (dolist (type (car arg-case))
      (unless (member type '(gs-integer
                             gs-array
                             gs-string
                             gs-block))
        (error "In definition of golfscript function \"~S\": ~S is not a valid golfscript type"
               name type))))
  `(setf (gethash (quote ,name) *variable-table*)
         (lambda ()
           ;; Coerce args if necessary
           ,(when (plusp coerce-n)
              `(loop for coerced-arg in 
                     (reverse (coerce-args
                                (loop repeat ,coerce-n collecting
                                      (stack-pop))))
                     do (stack-push coerced-arg)))
           (cond
             ;; Set up cond clauses for each arg type combination
             ,@(mapcar (lambda (arg-case)
                         `((equal (mapcar #'type-of
                                          (stack-peek ,(length (car arg-case))))
                                  (quote ,(car arg-case)))
                           ,@(cdr arg-case))) 
                       arg-cases)
             ;; Fallen through all possible combinations; invalid function call
             (t (error "~S called with invalid argument types; didn't match any of expected cases: ~S"
                       (quote ,name)
                       (quote ,(mapcar #'car arg-cases))))))))

#+nil
(define-gs-function ~ (a b)
                    )

#+nil
(define-gs-function (+ 2) 
                    ;; Test macro
  ((gs-string) do stuff)
  ((gs-integer) do other stuff)
  ((gs-array) do more stuff)
  ((gs-block) do even more stuff))
