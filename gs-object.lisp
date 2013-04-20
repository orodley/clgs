;;;; Everything to do with golfscript objects
(in-package :clgs)

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

(defun gs-repr (object)
  "Return the gs-string that returns OBJECT when eval'd in golfscript"
  (let ((value (gs-var-value object)))
    (flet ((vector-concat (start-character value end-character)
             (concatenate 'simple-vector
                      (vector (gs-integer<-char start-character))
                      value
                      (vector (gs-integer<-char end-character)))))
      (make-gs-string-from
        (etypecase object
          (gs-block
            (vector-concat #\{ value #\}))
          (gs-string
            (vector-concat #\' value #\'))
          (gs-array
            (vector-concat #\[
                      (if (zerop (length value))
                        #()
                        (reduce (lambda (a b)
                                (concatenate 'simple-vector
                                             a
                                             (vector (gs-integer<-char #\Space))
                                             (gs-var-value (gs-repr b))))
                              value
                              :initial-value (gs-var-value 
                                               (gs-repr (elt value 0)))
                              :start 1))
                      #\]))
          (gs-integer
            (map 'simple-vector #'gs-integer<-char
                 (write-to-string value))))))))

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
             (gs-string (make-gs-string-from (map 'simple-vector 
                                                  #'gs-integer<-char
                                                  (write-to-string value))))
             (gs-block  (make-gs-block-from (gs-var-value
                                              (coerce-gs-object object
                                                                'gs-string))))))
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
                                        (gs-var-value (coerce-gs-object
                                                        item
                                                        'gs-string)))))))))))))

(defun coerce-args (args)
  "Given a list of arguments, coerce all to highest priority type"
  (let ((highest-priority-type
          (type-of-priority
            (apply #'max (mapcar #'priority args)))))
    (mapcar (lambda (arg)
              (coerce-gs-object arg highest-priority-type))
            args)))

(defun gs-integer<-char (char)
  (make-gs-integer-from (char-code char)))

(defun char<-gs-integer (gs-int)
  (code-char (gs-var-value gs-int)))
