;;;; Runs all tests from "test-cases.lisp"

(loop for test-case in (with-open-file (file #P"test-cases.lisp")
                         (read file)) 
      do
      (let ((test-case-output ""))
        (handler-case
          (progn
            ;; Redirect stdout from EXECUTE-GS-PROGRAM
            (with-open-stream (*standard-output*
                                (make-string-output-stream))
              (execute-gs-program (car test-case))
              (setf test-case-output
                    (get-output-stream-string *standard-output*))) 
            (unless (string= (cadr test-case)
                             (string-trim '(#\Space #\Newline) 
                                          test-case-output))
              (format t "Output was different for ~A:~% ~
                         Expected~%  ~A~% Got~%  ~A~%~%"
                      (car test-case) (cadr test-case) test-case-output))) 
          (error (error)
                 (format t "~A raised:~% ~A~%~%" (car test-case) error))
          (warning (warning)
                   (format t "~A raised warning:~% ~A~%~%"
                           (car test-case) warning)))))
