(defvar *stack-mark* 0
  "The [ function uses this to mark stack size, and ] slices back to it")

(defvar *builtins* (make-hash-table)
  "Holds all builtin functions and variables")

(defmacro define-gs-function ((name &key (coerce 0) (require 0)) &body arg-cases)
  "Define a builtin function and insert it into *builtins*.
  Each case defines a different function to perform depending
  on the types of the arguments.
  Supertypes will shadow subtypes, so make sure to insert
  arg-cases in most specific type first order."
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
                       ;; Check for required amount of args
                       ,(when (plusp require)
                          `(unless (>= (length *stack*)
                                       ,require)
                             (error "Not enough values on stack for ~
                                    function ~S: expected >=~D, got ~D"
                                    (quote ,name) ,require (length *stack*))))
                       (cond
                         ;; Set up cond clauses for each arg type combination
                         ,@(mapcar (lambda (arg-case)
                                     (if (equal (car arg-case) '(t))
                                       `(t ,@(cdr arg-case))
                                       `((and
                                           (>= (length *stack*)
                                               ,(length (car arg-case)))
                                           (every #'typep 
                                                  (stack-peek ,(length (car arg-case))) 
                                                  (quote ,(car arg-case))))
                                         ,@(cdr arg-case)))) 
                                   arg-cases)
                         ;; Fallen through all possible combinations:
                         ;; invalid function call
                         (t (error "~S called with invalid argument types; didn't ~
                                   match any expected cases:~%~S"
                                   (quote ,name)
                                   (quote ,(mapcar #'car arg-cases))))))
  *builtins*))

(defmacro pop-into (var-list &body body)
  "Pop the top values of the stack into the variables in VAR-LIST
  in order, and bind their GS-VAR-VALUEs to <VAR>-VAL for each
  <VAR> in VAR-LIST, then execute body."
  ;; TODO: Remove unused bindings to avoid annoying "defined but never used" warnings
  ;; TODO: Possibly add (TYPE-OF VAR) => <VAR>-TYPE bindings?
  `(let* ,(append
           (mapcar (lambda (var)
                     `(,var (stack-pop)))
                   var-list)
           (mapcar (lambda (var)
                     `(,(intern
                          (concatenate 'string
                                       (string var)
                                       "-VAL"))
                        (gs-var-value ,var)))
                   var-list))
     ,@body))

(defun call-gs-fun (fun-symbol)
  "Call the function denoted by FUN-SYMBOL"
  (funcall (get-from-var-table fun-symbol *variable-table*)))

(defun split-gs-array (sequence delimiter)
  "Return a vector consisting of SEQUENCE split along DELIMITER"
  ;; TODO: Fix case where delimiter is at the end:
  ;; (split-gs-array "a|b|" "|") currently returns #("a" "b|")
  ;; rather than #("a" "b" "")
  (let* ((delim-length (length delimiter))
         (sequence-val (gs-var-value sequence))
         (sequence-length (length sequence-val)))
    (apply #'vector
           (loop with index = 0
                 with subseq-start = 0
                 while (< index (- sequence-length
                                   delim-length))
                   if (equalp delimiter
                              (subseq sequence-val index
                                      (+ index delim-length)))
                     collect (make-same-type
                               sequence
                               (subseq sequence-val subseq-start index))
                     and do (progn 
                              (incf index delim-length)
                              (setf subseq-start index))
                   else
                     do (incf index 1)
                   when (>= index
                            (- sequence-length
                               delim-length))
                     collect (make-same-type
                               sequence
                               (subseq sequence-val subseq-start
                                       sequence-length))))))

;;; Builtin functions
(define-gs-function (~ :require 1)
  ((gs-integer)
   ;; Bitwise not
   (pop-into (a)
     (stack-push (make-gs-integer
                   (lognot a-val))))) 
  ((gs-string) 
   ;; Eval
   (pop-into (a)
     (execute-gs-string a-val)))
  ((gs-array)
   ;; Dump items
   (pop-into (a)
     (map nil
          (lambda (object)
            (stack-push object))
          a-val))))

(define-gs-function (|`| :require 1)
  ((t)
   ;; Inverse of ~ like pythons `object`
   (pop-into (a)
     (stack-push
       (gs-repr a)))))

(define-gs-function (! :require 1)
  ((t)
   ;; Boolean NOT, with 0, [] and "" being false
   (pop-into (a)
     (stack-push
       (make-gs-integer
         (if (member a-val '(0 #() "")
                     :test #'equalp)
           1
           0))))))

(define-gs-function (@ :require 3)
  ((t)
   ;; Rotate top 3 stack elements
   ;; 2 3 4 => 3 4 2
   (pop-into (a b c)
     (stack-push b a c))))

(define-gs-function ($ :require 1)
  ((gs-integer)
   ;; nth element in stack
   (pop-into (a)
     (stack-push
       (stack-elt a-val)))) 
  ((gs-string)
   ;; Sort 
   (pop-into (string) 
     (stack-push
       (make-gs-string
         (sort string-val #'< :key #'gs-var-value)))))
  ((gs-array)
   (pop-into (array)
     (stack-push
       (make-gs-array
         (sort array-val #'< :key #'gs-var-value)))))
  ((gs-block)
   ;; Sort string or array by a mapping, like:
   ;; (SORT SEQUENCE :KEY KEY-BLOCK)
   ;; TODO: Doesn't sort strings correctly
   (pop-into (block sequence)
     (stack-push
       (make-same-type sequence
         (sort sequence-val #'<
               :key
               (lambda (x)
                 (stack-push x)
                 (execute-gs-string block-val)
                 (gs-var-value (stack-pop)))))))))

(define-gs-function (+ :coerce 2) 
  ((gs-integer)
   ;; Add
   (pop-into (a b)
     (stack-push (make-gs-integer
                   (+ b-val a-val)))))
  ((gs-array)
   (pop-into (a b)
     ;; Concatenate
     (stack-push (make-same-type a
                   (concatenate 'vector b-val a-val))))))

(define-gs-function (- :coerce 2)
  ((gs-integer)
   ;; Subtraction
   (pop-into (a b)
     (stack-push (make-gs-integer
                   (- b-val a-val)))))
  ((gs-array)
   ;; Array difference
   (pop-into (a b)
     (stack-push (make-gs-array
                   (delete-if (lambda (item)
                                (find item a-val
                                      :test #'equalp))
                              b-val))))))

(define-gs-function (* :require 2)
  ((gs-integer gs-integer)
   ;; Multiplication
   (pop-into (a b)
     (stack-push (make-gs-integer
                   (* b-val a-val)))))
  ((gs-block gs-integer)
   ;; Execute block n times
   (pop-into (block n)
     (loop repeat n-val do
           (execute-gs-string block-val))))
  ((gs-integer gs-block)
   (pop-into (n block)
     (loop repeat n-val do
           (execute-gs-string block-val))))
  ((gs-integer gs-array)
   ;; Array repeat
   (pop-into (n array)
     (stack-push
       (make-same-type 
         array
         (reduce (lambda (a b) 
                   (concatenate 'vector a b)) 
                 (loop repeat n-val collecting array-val))))))
  ((gs-block gs-array)
   ;; Reduce
   (pop-into (block array)
     (stack-push
       (reduce (lambda (a b)
                 (stack-push a)
                 (stack-push b)
                 (execute-gs-string block-val)
                 (stack-pop))
               array-val))))
  ((gs-array gs-array)
   ;; Join
   (pop-into (joiner joinee)
     (destructuring-bind (joinee joiner) (coerce-args `(,joinee ,joiner))
       (let ((joinee-val (gs-var-value joinee))
             (joiner-val (gs-var-value joiner)))
         (stack-push
           (make-same-type 
             joinee
             (reduce (lambda (a b)
                       (concatenate 'vector a joiner-val b))
                     joinee-val
                     :key #'vector))))))))

(define-gs-function (/ :require 2)
  ((gs-integer gs-integer)
   ;; Truncating integer division
   (pop-into (a b)
     (stack-push
       (make-gs-integer (truncate b-val a-val)))))
  ((gs-array gs-array)
   ;; Split array
   (pop-into (delimiter array)
     (stack-push
       (make-gs-array
         (split-gs-array array delimiter-val))))))

(define-gs-function (% :require 2)
  ((gs-integer gs-integer)
   ;; Modulus
   (pop-into (a b)
     (stack-push (make-gs-integer
                   (mod b-val a-val)))))
  ((gs-array gs-array)
   ;; Array split, with empty elements removed
   (pop-into (delimiter array)
     (stack-push
       (make-gs-array
         (remove (make-same-type array #()) 
                 (split-gs-array array delimiter-val)
                 :test #'equalp))))) 
  ((gs-integer gs-array)
   ;; Select every nth element; like pythons [::n]
   ;; Go from end if n is negative
   (pop-into (n array)
     (stack-push
       (make-gs-array
         (apply #'vector
                (cond
                  ((plusp n-val)
                   (loop for index below (length array) by n-val
                         collect (elt array-val index)))
                  ((minusp n)
                   (loop for index from (1- (length array-val)) downto 0 by (abs n-val)
                         collect (elt array-val index)))
                  (t (error "Zero slice value supplied to %"))))))))
  ((gs-integer gs-string)
   ;; TODO: Yucky code duplication
   (pop-into (n array)
     (stack-push
       (make-gs-string
         (coerce 
           (cond
             ((plusp n-val)
              (loop for index below (length array-val) by n-val
                    collect (elt array-val index)))
             ((minusp n-val)
              (loop for index from (1- (length array-val)) downto 0 by (abs n-val)
                    collect (elt array-val index)))
             (t (error "Zero slice value supplied to %")))
           'vector)))))
  ((gs-block gs-array)
   ;; Map
   (pop-into (block array)
     (call-gs-fun '[) 
     (loop for index below (length array-val) do
           (stack-push (elt array-val index))
           (execute-gs-string block-val))
     (call-gs-fun '])))
  ((gs-block gs-string)
   (pop-into (block string)
     (call-gs-fun '[)
     (loop for index below (length string-val) do
           (stack-push (elt string-val index))
           (execute-gs-string block-val))
     (call-gs-fun ']))))

(define-gs-function ([)
  ;; TODO: Fix for nested arrays
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

(define-gs-function (|\\| :require 2)
  ((t)
   ;; Swap top two stack elements
   (pop-into (a b)
     (stack-push a b))))

(define-gs-function (|;| :require 1)
  ((t)
   ;; Pop and discard top of stack
   (stack-pop)))

(define-gs-function (|,| :require 1)
  ((gs-integer)
   ;; Range
   (pop-into (a)
     (let ((range-vector (make-array a-val)))
       (loop for n below a-val do
             (setf (elt range-vector n)
                   (make-gs-integer n)))
       (stack-push (make-gs-array range-vector)))))
  ((gs-block gs-array)
   ;; Filter
   (pop-into (predicate array)
     (setf predicate-val
           (concatenate 'vector predicate-val
                        (vector (make-gs-integer 
                                  (char-code #\!)))))
     (stack-push
       (make-same-type
         array
         (delete-if (lambda (element)
                      (stack-push element)
                      (execute-gs-string predicate-val)
                      (not (zerop
                             (gs-var-value (stack-pop)))))
                    array-val)))))
  ((gs-array)
   ;; Array size
   (pop-into (a)
     (stack-push (make-gs-integer
                   (length a-val))))))

(define-gs-function (|.| :require 1)
  ((t)
   ;; Duplicate top of stack
   (pop-into (a)
     (stack-push
       (make-same-type a a-val)))))

;;                    |- Fucks up parenthesis balancing in slimv
;;                    |  Tricks it into accepting the form as balanced
;;                    v                     v
(define-gs-function (|(| :require 1) #+nil |)|
  ((gs-integer)
   ;; Decrement
   (pop-into (a)
     (stack-push
       (make-gs-integer (1- a-val)))))
  ((gs-array)
   ;; "Uncons"
   (pop-into (array)
     (stack-push
       (make-gs-array
         (subseq array-val 1))
       (elt array 0)))))

;; And again, trick slimv -v
(define-gs-function #+nil |(| (|)| :require 1)
  ((gs-integer)
   ;; Increment
   (pop-into (a)
     (stack-push
       (make-gs-integer (1+ a-val)))))
  ((gs-array)
   ;; "Uncons" from right
   (pop-into (array)
     (let ((array-end (1- (length array-val))))
       (stack-push
         (make-same-type array
           (subseq array-val 0 array-end))
         (elt array-val array-end))))))
