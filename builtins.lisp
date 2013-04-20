(in-package :clgs)

(defvar *stack-mark* ()
  "The [ function uses this to mark stack size, and ] slices back to it")

(defvar *builtins* (make-hash-table)
  "Holds all builtin functions and variables")

(defmacro define-gs-function ((name &key (coerce 0) (require 0))
                              &body arg-cases)
  "Define a builtin function and insert it into *builtins*.  Each case defines
   a different function to perform depending on the types of the arguments.
   Supertypes will shadow subtypes, so make sure to insert arg-cases in most
   specific type first order.  Vertical bars are necessary for names containing
   alphabetical characters, so that the reader doesn't upcase them"
  ;; Check for invalid types in ARG-CASES
  (dolist (arg-case arg-cases)
    (dolist (type (car arg-case))
      (unless (member type '(gs-integer gs-array gs-string gs-block t))
        (error "In definition of golfscript function `~S': ~S is not a valid ~
                golfscript type"
               name type))))
  `(add-to-var-table
     ',name
     (lambda ()
       (declare (type list *stack*))
       ;; Coerce args if necessary
       ,(when (plusp coerce)
          `(loop for coerced-arg in 
                 (reverse (coerce-args 
                            (loop repeat ,coerce collecting (stack-pop))))
                 do (stack-push coerced-arg)))
       ;; Check for required amount of args
       ,(when (plusp require)
          `(unless (>= (length *stack*) ,require)
             (error "Not enough values on stack for  function ~S: ~
                    expected >=~D, got ~D"
                    ',name ,require (length *stack*))))
     (cond
       ;; Set up cond clauses for each arg type combination
       ,@(mapcar
           (lambda (arg-case)
             `(,(if (equal (car arg-case) '(t))
                  t
                  `(and (>= (length *stack*) ,(length (car arg-case)))
                        (every #'typep (stack-peek ,(length (car arg-case))) 
                               ',(car arg-case)))) 
                ,@(if (eql (caadr arg-case) 'pop-into)
                    `(,(pop-into-expansion  arg-case))
                    (cdr arg-case)))) 
           arg-cases)
       ;; Fallen through all possible combinations: invalid function call
       (t (error "~S called with invalid argument types; didn't ~
                 match any expected cases:~%~S"
                 ',name ',(mapcar #'car arg-cases)))))
           *builtins*))

(defun pop-into-expansion (arg-case)
  "Generate the expansion for the POP-INTO macro; pop the top values of the
   stack into the vars in the var list in order, bind their GS-VAR-VALUEs to
   <VAR>-VAL for each <VAR> in the var list, then execute the body."
  ;; TODO: Remove unused bindings to avoid
  ;; annoying "defined but never used" warnings
  ;; Or maybe just (DECLARE (IGNORABLE ...)) them?
  (let ((var-list (cadadr arg-case))
        (body     (cddadr arg-case)))
    (declare (type list var-list body))
    `(let* ,(append
             (mapcar (lambda (var) `(,var (stack-pop)))
                     var-list)
             (mapcar (lambda (var) `(,(intern (format nil "~S-VAL" var))
                                      (gs-var-value ,var)))
                     var-list))
       ,@(loop for var  in var-list
               for type in (car arg-case)
               collecting `(declare (type ,type ,var)))
       ,@(loop for var  in var-list
               for type in (car arg-case)
               collecting
               `(declare (type ,(case type
                                  (gs-integer 'integer)
                                  ((t)        't)
                                  (otherwise  'simple-vector))
                               ,(intern (format nil "~S-VAL" var)))))
       ,@body)))

(defun call-gs-fun (fun-symbol)
  "Call the function denoted by FUN-SYMBOL"
  (funcall (get-from-var-table fun-symbol *variable-table*)))

(defun split-gs-array (gs-array delimiter)
  "Return a vector consisting of GS-ARRAY split along DELIMITER"
  ;; TODO: Fix case where delimiter is at the end:
  ;; (SPLIT-GS-ARRAY "a|b|" "|") currently returns #("a" "b|")
  ;; rather than #("a" "b" "")
  (let* ((delim-length (length delimiter))
         (gs-array-val (gs-var-value gs-array))
         (gs-array-length (length gs-array-val)))
    (apply #'vector
           (loop with index = 0
                 with subseq-start = 0
                 while (< index (- gs-array-length
                                   delim-length))
                   if (equalp delimiter
                              (subseq gs-array-val index
                                      (+ index delim-length)))
                     collect (make-same-type
                               gs-array
                               (subseq gs-array-val subseq-start index))
                     and do (progn 
                              (incf index delim-length)
                              (setf subseq-start index))
                   else
                     do (incf index 1)
                   when (>= index
                            (- gs-array-length
                               delim-length))
                     collect (make-same-type
                               gs-array
                               (subseq gs-array-val subseq-start
                                       gs-array-length))))))

;;; Builtin functions
(define-gs-function (~ :require 1)
  ((gs-integer)
   ;; Bitwise not
   (pop-into (a)
     (stack-push (make-gs-integer-from
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
   ;; Inverse of ~ like pythons `object` or repr(object)
   (pop-into (a)
     (stack-push
       (gs-repr a)))))

(define-gs-function (! :require 1)
  ((t)
   ;; Boolean NOT, with 0, [] and "" being false
   (pop-into (a)
     (stack-push
       (make-gs-integer-from
         (if (truth-value a) 0 1))))))

(define-gs-function (@ :require 3)
  ((t t t)
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
  ((gs-block gs-array)
   ;; Sort array by a mapping, like (SORT SEQUENCE :KEY KEY-BLOCK)
   (pop-into (block array)
     (stack-push
       (make-same-type array
                       (sort array-val #'<
                             :key
                             (lambda (x)
                               (stack-push x)
                               (execute-gs-string block-val)
                               (gs-var-value (stack-pop))))))))
  ((gs-array)
   ;; Sort 
   (pop-into (array) 
     (stack-push
       (make-same-type
         array
         (sort array-val #'< :key #'gs-var-value))))))

(define-gs-function (+ :coerce 2) 
  ((gs-integer gs-integer)
   ;; Add
   (pop-into (a b)
     (stack-push (make-gs-integer-from
                   (+ b-val a-val)))))
  ((gs-block gs-block)
   ;; Concatenate with space
   (pop-into (a b)
     (stack-push (make-same-type
                   a
                   (concatenate 'simple-vector
                                b-val
                                (vector (gs-integer<-char #\Space))
                                a-val)))))
  ((gs-array gs-array)
   ;; Concatenate without space
   (pop-into (a b)
     (stack-push (make-same-type a (concatenate 'simple-vector b-val a-val)))))
  ((gs-array)
   ;; Convert to string
   (pop-into (array)
     (stack-push (make-gs-string-from array-val)))))

(define-gs-function (- :coerce 2)
  ((gs-integer gs-integer)
   ;; Subtraction
   (pop-into (a b)
     (stack-push (make-gs-integer-from
                   (- b-val a-val)))))
  ((gs-array gs-array)
   ;; Array difference
   (pop-into (a b)
     (stack-push (make-gs-array-from
                   (delete-if (lambda (item)
                                (find item a-val
                                      :test #'equalp))
                              b-val))))))

(define-gs-function (* :require 2)
  ((gs-integer gs-integer)
   ;; Multiplication
   (pop-into (a b)
     (stack-push (make-gs-integer-from
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
                   (concatenate 'simple-vector a b)) 
                 (loop repeat n-val collecting array-val))))))
  ((gs-array gs-integer)
   (pop-into (array n)
     (stack-push
       (make-same-type 
         array
         (reduce (lambda (a b) 
                   (concatenate 'simple-vector a b)) 
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
                       (concatenate 'simple-vector a joiner-val b))
                     joinee-val
                     :key #'vector))))))))

(define-gs-function (/ :require 2)
  ((gs-integer gs-integer)
   ;; Truncating integer division
   (pop-into (a b)
     (stack-push
       (make-gs-integer-from (truncate b-val a-val)))))
  ((gs-integer gs-array)
   ;; Split into groups of size n
   (pop-into (n array)
     (let ((array-length (length array-val)))
       (stack-push
         (make-gs-array-from
           (apply #'vector
                  (loop for index below array-length by n-val
                        collecting (make-same-type
                                     array
                                     (subseq array-val index
                                             (if (<= (+ index n-val)
                                                     array-length)
                                               (+ index n-val)
                                               array-length))))))))))
  ((gs-block gs-block)
   ;; "Unfold"
   (pop-into (body condition)
     (stack-push
       (make-gs-array-from
         (apply #'vector
                (loop while (progn
                              (call-gs-fun '|.|)
                              (execute-gs-string condition-val)
                              (truth-value (stack-pop)))
                      collect (stack-elt 0)
                      do (execute-gs-string body-val)))))
     (call-gs-fun '|\\|)
     (call-gs-fun '|;|)))
  ((gs-block gs-array)
   ;; foreach/dolist loop
   (pop-into (block array)
     (map nil (lambda (item)
                (stack-push item)
                (execute-gs-string block-val))
          array-val)))
  ((gs-array gs-array)
   ;; Split array
   (pop-into (delimiter array)
     (stack-push
       (make-gs-array-from
         (split-gs-array array delimiter-val))))))

(define-gs-function (% :require 2)
  ((gs-integer gs-integer)
   ;; Modulus
   (pop-into (a b)
     (stack-push (make-gs-integer-from
                   (mod b-val a-val)))))
  ((gs-block gs-array)
   ;; Map
   (pop-into (block array)
     (call-gs-fun '[) 
     (loop for index below (length array-val) do
           (stack-push (elt array-val index))
           (execute-gs-string block-val))
     (call-gs-fun '])
     (stack-push
       (make-same-type
         array
         (gs-var-value (stack-pop))))))
  ((gs-array gs-array)
   ;; Array split, with empty elements removed
   (pop-into (delimiter array)
     (stack-push
       (make-gs-array-from
         (remove (make-same-type array #()) 
                 (split-gs-array array delimiter-val)
                 :test #'equalp))))) 
  ((gs-integer gs-array)
   ;; Select every nth element; like pythons [::n]
   ;; Go from end if n is negative
   (pop-into (n array)
     (stack-push
       (make-same-type
         array
         (apply #'vector
                (cond
                  ((plusp n-val)
                   (loop for index below (length array-val) by n-val
                         collect (elt array-val index)))
                  ((minusp n-val)
                   (loop for index from (1- (length array-val)) downto 0 by (abs n-val)
                         collect (elt array-val index)))
                  (t (error "Zero slice value supplied to %")))))))))

(define-gs-function (|\|| :coerce 2)
  ((gs-integer gs-integer)
  ;; Bitwise OR
  (pop-into (a b)
    (stack-push
      (make-gs-integer-from
        (logior b-val a-val)))))
  ((gs-array gs-array)
   ;; Set union
   ;; UNION can't be used, as order is undefined
   (pop-into (array2 array1)
     (stack-push
       (make-same-type
         array1
         (concatenate 'simple-vector
                      (remove-duplicates array1-val :test #'equalp)
                      (loop for item across
                            (remove-duplicates array2-val :test #'equalp)
                            unless (find item array1-val :test #'equalp)
                              collect item)))))))

(define-gs-function (& :coerce 2)
  ((gs-integer gs-integer)
   ;; Bitwise AND
   (pop-into (a b)
     (stack-push
       (make-gs-integer-from
         (logand b-val a-val)))))
  ((gs-array gs-array)
   ;; Set intersection
   ;; INTERSECTION can't be used, as order is undefined
   (pop-into (array1 array2)
     (stack-push
       (make-same-type
         array1
         (apply #'vector
                (loop for item across
                      (remove-duplicates array1-val :test #'equalp)
                      when (find item array2-val :test #'equalp)
                        collect item)))))))

(define-gs-function (^ :coerce 2)
  ((gs-integer gs-integer)
   ;; Bitwise XOR
   (pop-into (a b)
     (stack-push
       (make-gs-integer-from
         (logxor b-val a-val)))))
  ((gs-array gs-array)
   ;; Set difference
   ;; SET-DIFFERENCE can't be used, as order is undefined
   (pop-into (array1 array2)
     (stack-push
       (let ((array1-no-dupes (remove-duplicates array1-val :test #'equalp))
             (array2-no-dupes (remove-duplicates array2-val :test #'equalp)))
         (declare (simple-vector array1-no-dupes array2-no-dupes))
         (make-same-type
           array1
           (concatenate 'simple-vector
                        (loop for item across array2-no-dupes
                              unless (find item array1-no-dupes :test #'equalp)
                              collect item)
                        (loop for item across array1-no-dupes
                              unless (find item array2-no-dupes :test #'equalp)
                              collect item))))))))

(define-gs-function ([)
  ((t)
   ;; Mark stack size
   (push (length *stack*) *stack-mark*)))

(define-gs-function (])
  ((t)
   ;; Slice stack back to mark
   (stack-push
     (make-gs-array-from
       (apply #'vector
              (nreverse
                (loop repeat (- (length *stack*)
                                (or (pop *stack-mark*)
                                    0))
                      collecting (stack-pop))))))))

(define-gs-function (|\\| :require 2)
  ((t t)
   ;; Swap top two stack elements
   (pop-into (a b)
     (stack-push a b))))

(define-gs-function (|;| :require 1)
  ((t)
   ;; Pop and discard top of stack
   (stack-pop)))

(define-gs-function (< :require 2)
  ((gs-integer gs-array)
   ;; Select array elements with index < n
   ;; Negative values select from end, like pythons [:n:]
   (pop-into (n array)
     (when (minusp n-val)
       (setf n-val (+ (length array-val) n-val)))
     (stack-push
       (make-same-type
         array
         (if (< -1 n-val (length array-val))
           (subseq array-val 0 n-val)
           #())))))
  ((gs-integer gs-integer)
   ;; Less-than comparison
   (pop-into (a b)
     (stack-push
       (make-gs-integer-from
         (if (< b-val a-val)
           1
           0)))))
  ((gs-array gs-array)
   (pop-into (array1 array2)
     (stack-push
       (make-gs-integer-from
         (if (some (lambda (a b)
                     (< (the integer (gs-var-value a))
                        (the integer (gs-var-value b))))
                   array2-val array1-val)
           1
           0))))))

(define-gs-function (> :require 2)
  ((gs-integer gs-array)
   ;; Select array elements with index > n
   ;; Negative values select from end, like pythons [n::]
   (pop-into (n array)
     (when (minusp n-val)
       (setf n-val (+ (length array-val) n-val)))
     (stack-push
       (make-same-type
         array
         (if (< -1 n-val (length array-val))
           (subseq array-val n-val (length array-val))
           #())))))
  ((gs-integer gs-integer)
   ;; Greater-than comparison
   (pop-into (a b)
     (stack-push
       (make-gs-integer-from
         (if (> b-val a-val)
           1
           0)))))
  ((gs-array gs-array)
   (pop-into (array1 array2)
     (stack-push
       (make-gs-integer-from
         (if (some (lambda (a b)
                     (> (gs-var-value a)
                        (gs-var-value b)))
                   array2-val array1-val)
           1
           0))))))

(define-gs-function (= :require 2)
  ((gs-integer gs-array)
   ;; Select array element with index = n
   ;; Negative values select from end, like pythons [n]
   ;; If the index is out of bounds, no error is thrown
   (pop-into (n array)
     (when (minusp n-val)
       (setf n-val (+ (length array-val) n-val)))
     (when (< -1 n-val (length array-val))
       (stack-push
         (elt array-val n-val)))))
  ((t)
   ;; Equality comparison for same types
   ;; Top two values are discarded for different types
   (pop-into (a b)
     (when (eq (type-of a)
               (type-of b))
       (stack-push
         (make-gs-integer-from
           (if (equalp a b)
             1
             0)))))))

(define-gs-function (|,| :require 1)
  ((gs-integer)
   ;; Range
   (pop-into (a)
     (let ((range-vector (make-array a-val)))
       (loop for n below a-val do
             (setf (elt range-vector n)
                   (make-gs-integer-from n)))
       (stack-push (make-gs-array-from range-vector)))))
  ((gs-block gs-array)
   ;; Filter
   (pop-into (predicate array)
     (stack-push
       (make-same-type
         array
         (delete-if (lambda (element)
                      (stack-push element)
                      (execute-gs-string predicate-val)
                      (not (truth-value (stack-pop))))
                    array-val)))))
  ((gs-array)
   ;; Array size
   (pop-into (a)
     (stack-push (make-gs-integer-from
                   (length a-val))))))

(define-gs-function (|.| :require 1)
  ((t)
   ;; Duplicate top of stack
   (pop-into (a)
     (stack-push
       a
       (make-same-type a a-val)))))

(define-gs-function (? :require 2)
  ((gs-integer gs-integer)
   ;; Raise an integer to a power
   (pop-into (a b)
     (stack-push
       (make-gs-integer-from
         (expt b-val a-val)))))
  ((gs-block gs-array)
   ;; FIND-IF
   (pop-into (predicate array)
     (stack-push
       (find-if (lambda (item)
                  (stack-push item)
                  (execute-gs-string predicate-val)
                  (truth-value (stack-pop)))
                array-val))))
  ((gs-array t)
   ;; POSITION
   (pop-into (array item)
     (stack-push
       (make-gs-integer-from
         (or (position item array-val :test #'equalp)
             -1))))))

;;                    |- Fucks up parenthesis balancing in slimv
;;                    |  Tricks it into accepting the form as balanced
;;                    v                     v
(define-gs-function (|(| :require 1) #+nil |)|
  ((gs-integer)
   ;; Decrement
   (pop-into (a)
     (stack-push
       (make-gs-integer-from (1- a-val)))))
  ((gs-array)
   ;; "Uncons"
   (pop-into (array)
     (stack-push
       (make-same-type
         array
         (subseq array-val 1))
       (elt array-val 0)))))

;; And again, trick slimv -v
(define-gs-function #+nil |(| (|)| :require 1)
  ((gs-integer)
   ;; Increment
   (pop-into (a)
     (stack-push
       (make-gs-integer-from (1+ a-val)))))
  ((gs-array)
   ;; "Uncons" from right
   (pop-into (array)
     (let ((array-end (1- (length array-val))))
       (stack-push
         (make-same-type array
           (subseq array-val 0 array-end))
         (elt array-val array-end))))))

(define-gs-function (|and| :require 2)
  ((t t)
   ;; Short-circuiting boolean and
   (stack-push (stack-elt 1))
   (call-gs-fun '|if|)))

(define-gs-function (|or| :require 2)
  ((t t)
  ;; Short-circuting bollean or
  (stack-push (stack-elt 1)) 
  (call-gs-fun '|\\|)
  (call-gs-fun '|if|)))

(define-gs-function (|xor| :require 2)
  ((t t)
   ;; Boolean exclusive or
   (pop-into (a b)
     (stack-push
       (if (truth-value a)
         (if (truth-value b)
           (make-gs-integer-from 0)
           a)
         (if (truth-value b)
           b
           (make-gs-integer-from 0)))))))

(define-gs-function (|rand| :require 1)
  ((gs-integer)
   ;; Random integer from 0 below n
   (pop-into (n)
     (stack-push
       (make-gs-integer-from
         (random n-val))))))

(define-gs-function (|do| :require 1)
  ((gs-block)
   ;; do {...} while loop
   (pop-into (predicate)
     (loop do (execute-gs-string predicate-val)
           unless (truth-value (stack-pop))
             return nil))))

(define-gs-function (|while| :require 2)
  ((gs-block gs-block)
   ;; while loop
   (pop-into (body predicate)
     (loop while
           (progn
             (execute-gs-string predicate-val)
             (truth-value (stack-pop)))
           do (execute-gs-string body-val)))))

(define-gs-function (|until| :require 2)
  ((gs-block gs-block)
   ;; until loop
   (pop-into (body predicate)
     (loop until
           (progn
             (execute-gs-string predicate-val)
             (truth-value (stack-pop)))
           do (execute-gs-string body-val)))))

(define-gs-function (|if| :require 3)
  ((t t t)
   ;; if statement
   (pop-into (else if condition)
     (let ((choice
             (if (truth-value condition) if else)))
       (if (typep choice 'gs-block)
         (execute-gs-string (gs-var-value choice))
         (stack-push choice))))))

(define-gs-function (|abs| :require 1)
  ((gs-integer)
   ;; Absolute value
   (pop-into (a)
     (stack-push
       (make-gs-integer-from
         (abs a-val))))))

(define-gs-function (|zip| :require 1)
  ((gs-array)
   ;; Transpose rows and columns of arrays
   ;; In the case of mixed types, output defaults
   ;; to the type of the first item in the array
   (pop-into (array)
     (when (every (lambda (item) (typep item 'gs-array)) array-val)
       (let ((output-spec (elt array-val 0)))
         (stack-push 
           (make-gs-array-from
             (apply #'map 'simple-vector
                    (lambda (&rest items)
                      (make-same-type
                        output-spec
                        (apply #'vector items)))
                    (map 'list #'gs-var-value array-val)))))))))

(define-gs-function (|base| :require 2)
  ((gs-integer gs-array)
   ;; Base conversion
   (pop-into (radix digit-array)
     (stack-push
       (make-gs-integer-from
         (let ((*read-base* radix-val))
           (read-from-string
             (map 'string
                  (lambda (digit)
                    (character
                      (write-to-string (gs-var-value digit))))
                  digit-array-val)))))))
  ((gs-integer gs-integer)
   (pop-into (radix number)
     (stack-push
       (make-gs-array-from
         (map 'simple-vector
              (lambda (digit)
                (make-gs-integer-from
                  (read-from-string (string digit))))
              (write-to-string number-val :base radix-val)))))))
