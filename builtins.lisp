(defvar *stack-mark* 0
  "The [ function uses this to mark stack size, and ] slices back to it")

(defvar *builtins* (make-hash-table)
  "Holds all builtin functions and variables")

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
                       ;; Check for required amount of args
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
                                       `((every (mapcar #'subtypep
                                                        (stack-peek ,(length (car arg-case)))
                                                        (quote ,(car arg-case))))
                                         ,@(cdr arg-case)))) 
                                   arg-cases)
                         ;; Fallen through all possible combinations; invalid function call
                         (t (error "~S called with invalid argument types; didn't ~
                                   match any expected cases:~%~S"
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

(defun split-sequence (sequence delimiter)
  "Return a vector consisting of SEQUENCE split along DELIMITER"
  ;; TODO: Fix case where delimiter is at the end:
  ;; (SPLIT-SEQUENCE "a|b|" "|") currently returns #("a" "b|")
  (let ((delim-length (length delimiter))
        (sequence-length (length sequence)))
    (apply #'vector
           (loop with index = 0
                 with subseq-start = 0
                 while (< index (- sequence-length
                                   delim-length))
                   if (equalp delimiter
                              (subseq sequence index
                                      (+ index delim-length)))
                     collect (subseq sequence subseq-start index)
                     and do (progn 
                              (incf index delim-length)
                              (setf subseq-start index))
                   else
                     do (incf index 1)
                   when (>= index
                            (- sequence-length
                               delim-length))
                     collect (subseq sequence subseq-start
                                     sequence-length)))))

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
   ;; Boolean NOT, with 0, [] and "" being false
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

(define-gs-function ($ :require 1)
  ((gs-integer)
   ;; nth element in stack
   (pop-into (a)
     (stack-push
       (stack-elt a)))) 
  ((gs-string)
   ;; Sort 
   (pop-into (string) 
     (stack-push
       (make-gs-string
         (sort string #'char<)))))
  ((gs-array)
   (pop-into (array)
     (stack-push
       (make-gs-array
         (sort array #'< :key #'gs-var-value)))))
  ((gs-block)
   ;; Sort string or array by a mapping, like:
   ;; (SORT SEQUENCE :KEY KEY-BLOCK)
   ;; TODO: Doesn't sort strings correctly
   (pop-into (block sequence)
     (let ((gs-string-p (typep sequence 'gs-string)))
       (stack-push
         (make-gs-object
           (sort sequence #'<
                 :key
                 (lambda (x)
                   (stack-push (if gs-string-p 
                                 (make-gs-integer
                                   (char-code x))
                                 x))
                   (execute-gs-string block)
                   (gs-var-value (stack-pop))))))))))

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

(define-gs-function (% :require 2)
  ((gs-integer gs-integer)
   ;; Modulus
   (pop-into (a b)
     (stack-push (make-gs-integer
                   (mod b a)))))
  ((gs-string gs-string)
   ;; String split, with empty elements removed
   (pop-into (delimiter string)
     (stack-push
       (make-gs-array
         (remove ""
                 (split-sequence string delimiter)
                 :test #'string-equal)))))
  ((gs-array gs-array)
   ;; Array split, with empty elements removed
   (pop-into (delimiter array)
     (stack-push
       (make-gs-array
         (remove #()
                 (split-sequence array delimiter)
                 :test #'equalp))))) 
  ((gs-integer gs-array)
   ;; Select every nth element; like pythons [::n]
   ;; Go from end if n is negative
   (pop-into (n array)
     (stack-push
       (make-gs-array
         (apply #'vector
                (cond
                  ((plusp n)
                   (loop for index below (length array) by n
                         collect (elt array index)))
                  ((minusp n)
                   (loop for index from (1- (length array)) downto 0 by (abs n)
                         collect (elt array index)))
                  (t (error "Zero slice value supplied to %"))))))))
  ((gs-integer gs-string)
   ;; TODO: Yucky code duplication
   (pop-into (n array)
     (stack-push
       (make-gs-string
         (coerce 
           (cond
             ((plusp n)
              (loop for index below (length array) by n
                    collect (elt array index)))
             ((minusp n)
              (loop for index from (1- (length array)) downto 0 by (abs n)
                    collect (elt array index)))
             (t (error "Zero slice value supplied to %")))
           'string)))))
  ((gs-block gs-array)
   ;; Map
   (pop-into (block array)
     (call-gs-fun '[) 
     (loop for index below (length array) do
           (stack-push (elt array index))
           (execute-gs-string block))
     (call-gs-fun '])))
  ((gs-block gs-string)
   (pop-into (block string)
     (call-gs-fun '[)
     (loop for index below (length string) do
           (stack-push (make-gs-integer
                         (char-code (elt string index))))
           (execute-gs-string block))
     (call-gs-fun '])
     (stack-push
       (map 'string (lambda (x)
                      (code-char (gs-var-value x)))
            (gs-var-value (stack-pop)))))))

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

(define-gs-function (|\\| :require 2)
  ((t)
   ;; Swap top two stack elements
   (pop-into* (a b)
     (stack-push a b))))

(define-gs-function (|;| :require 1)
  ((t)
   ;; Pop and discard top of stack
   (stack-pop)))

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

(define-gs-function (|.| :require 1)
  ((t)
   ;; Duplicate top of stack
   (stack-push
     (make-gs-object
       (gs-var-value
         (stack-elt 0))))))

;;                    |- Fucks up parenthesis balancing in slimv
;;                    |  Tricks it into accepting the form as balanced
;;                    v                     v
(define-gs-function (|(| :require 1) #+nil |)|
  ((gs-integer)
   ;; Decrement
   (pop-into (a)
     (stack-push
       (make-gs-integer (1- a)))))
  ((gs-array)
   ;; "Uncons"
   (pop-into (array)
     (stack-push
       (make-gs-array
         (subseq array 1))
       (elt array 0)))))

;; And again, trick slimv -v
(define-gs-function #+nil |(| (|)| :require 1)
  ((gs-integer)
   ;; Increment
   (pop-into (a)
     (stack-push
       (make-gs-integer (1+ a)))))
  ((gs-array)
   ;; "Uncons" from right
   (pop-into (array)
     (let ((array-end (1- (length array))))
       (stack-push
         (make-gs-array
           (subseq array 0 array-end))
         (elt array array-end))))))
