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
