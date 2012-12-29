;;;; Golfscript interpreter

(ql:quickload 'cl-ppcre)

;;; Stack & related functions
(defvar *stack* ()
  "Main golfscript stack.")

(defvar *stack-mark* 0
  "The [ function uses this to mark stack size, and ] slices back to it")

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

(defvar *builtins* (make-hash-table)
  "Holds all builtin functions and variables")

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

;; TODO: Add arg number checking for individual arg cases
(defmacro define-gs-function ((name &key (coerce 0) (require 0)) &body arg-cases)
  "Define a builtin function and insert it into *builtins*.
  Each case defines a different function to perform depending
  on the types of the arguments."
  ;; Check for invalid types in ARG-CASES
  (dolist (arg-case arg-cases)
    (dolist (type (car arg-case))
      (unless (member type '(gs-integer
                             gs-array
                             gs-string
                             gs-block
                             t))
        (error "In definition of golfscript function \"~S\": ~S is not ~
               a valid golfscript type"
               name type))))
  `(add-to-var-table (quote ,name)
                     (lambda ()
                       ;; Coerce args if necessary
                       ,(when (plusp coerce)
                          `(loop for coerced-arg in 
                                 (reverse (coerce-args
                                            (loop repeat ,coerce collecting
                                                  (stack-pop))))
                                 do (stack-push coerced-arg)))
                       ,(when (plusp require)
                          `(unless (>= (length *stack*)
                                       ,require)
                             (error "Not enough values on stack for function ~S: ~
                                    expected >=~D, got ~D"
                                    (quote ,name) ,require (length *stack*))))
                       (cond
                         ;; Set up cond clauses for each arg type combination
                         ,@(mapcar (lambda (arg-case)
                                     (if (equal (car arg-case) '(t))
                                       `(t ,@(cdr arg-case))
                                       `((equal (mapcar #'type-of
                                                        (stack-peek ,(length (car arg-case))))
                                                (quote ,(car arg-case)))
                                         ,@(cdr arg-case)))) 
                                   arg-cases)
                         ;; Fallen through all possible combinations; invalid function call
                         (t (error "~S called with invalid argument types; didn't ~
                                   match any of expected cases: ~S"
                                   (quote ,name)
                                   (quote ,(mapcar #'car arg-cases))))))
                     *builtins*))

(defmacro pop-into (var-list &body body)
  "Pop the GS-VAR-VALUEs of the top values of the stack into
  the variables in VAR-LIST in order, then execute body"
  `(let ,(mapcar (lambda (var)
                   `(,var (gs-var-value (stack-pop))))
                 var-list)
     ,@body))

(defmacro pop-into* (var-list &body body)
  "Pop the top values of the stack into the variables in
  VAR-LIST in order, then execute body"
  `(let ,(mapcar (lambda (var)
                   `(,var (stack-pop)))
                 var-list)
     ,@body))

(defun call-gs-fun (fun-symbol)
  "Call the function denoted by FUN-SYMBOL"
  (funcall (get-from-var-table fun-symbol *variable-table*)))

;;; Builtin functions
(define-gs-function (~ :require 1)
  ((gs-integer)
   ;; Bitwise not
   (pop-into (a)
     (stack-push (make-gs-integer
                   (lognot a))))) 
  ((gs-string) 
   ;; Eval
   (pop-into (a)
     (execute-gs-string a))) 
  ((gs-block)
   (pop-into (a)
     (execute-gs-string a)))
  ((gs-array)
   ;; Dump items
   (pop-into (a)
     (map nil
          (lambda (object)
            (stack-push object))
          a))))

(define-gs-function (! :require 1)
  ((t)
   ;; Boolean NOT
   (pop-into (a)
     (stack-push
       (make-gs-integer
         (if (member a '(0 #() "")
                     :test #'equalp)
           1
           0))))))

(define-gs-function (@ :require 3)
  ((t)
   ;; Rotate top 3 stack elements
   ;; 2 3 4 => 3 4 2
   (pop-into* (a b c)
     (stack-push b a c))))

(define-gs-function ($)
  ((gs-integer)
   ;; nth element in stack
   (pop-into (a)
     (stack-push
       (stack-elt a)))) 
  ((gs-string)
   ;; Sort 
   (pop-into (a) 
   (stack-push
     (make-gs-string
       (sort a #'char<)))))
  ((gs-block gs-string)
   (pop-into (key-block string)
     (stack-push
       (make-gs-string
         ())))))

(define-gs-function (+ :coerce 2) 
  ((gs-integer)
   ;; Add
   (pop-into (a b)
     (stack-push (make-gs-integer
                   (+ b a)))))
  ((gs-string)
   ;; Concatenate
   (pop-into (a b)
     (stack-push (make-gs-string
                   (concatenate 'string b a)))))
  ((gs-array)
   (pop-into (a b)
     (stack-push (make-gs-array
                   (concatenate 'vector b a)))))
  ((gs-block)
   (pop-into (a b)
     (stack-push (make-gs-block
                   (concatenate 'string b a))))))

(define-gs-function ([)
  ((t)
   ;; Mark stack size
   (setf *stack-mark* (length *stack*))))

(define-gs-function (])
  ((t)
   ;; Slice stack back to mark
   (stack-push
     (make-gs-array
       (apply #'vector
              (nreverse
                (loop repeat (- (length *stack*)
                                *stack-mark*)
                      collecting (stack-pop))))))))

(define-gs-function (|,| :require 1)
  ((gs-integer)
   ;; Range
   (pop-into (a)
     (let ((range-vector (make-array a)))
       (loop for n below a do
             (setf (elt range-vector n)
                   (make-gs-integer n)))
       (stack-push (make-gs-array range-vector)))))
  ((gs-array)
   ;; Array size
   (pop-into (a)
     (stack-push (make-gs-integer
                   (length a)))))
  ((gs-block gs-array)
   ;; Filter
   (pop-into (predicate array)
     (let ((predicate
             (concatenate 'string predicate "!"))
           (filtered-array
             (make-array 0 :fill-pointer t)))
       (map nil
            (lambda (object)
              (stack-push object))
            array) 
       (loop repeat (length array) do
             (execute-gs-string predicate)
             (if (zerop (gs-var-value (stack-elt 0)))
               (vector-push-extend (stack-pop) filtered-array)      
               (stack-pop)))
       (stack-push (make-gs-array filtered-array))))))
