;;;; Runs all tests from "test-cases.lisp"

(loop for test-case in (with-open-file (file #P"test-cases.lisp")
                         (read file)) 
      with errors   = 0
      with failures = 0
      with warnings = 0
      for test-case-output = ""
      do
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
            (incf failures)
            (format t "Output was different for ~A:~% ~
                       Expected~%  ~A~% Got~%  ~A~%~%"
                    (car test-case) (cadr test-case) test-case-output))) 
        (error (error)
               (incf errors)
               (format t "~A raised:~% ~A~%~%" (car test-case) error))
        (warning (warning)
                 (incf warnings)
                 (format t "~A raised warning:~% ~A~%~%"
                         (car test-case) warning)))

      finally (format t "Total errors:   ~D~%Total failures: ~D~%Total ~
                         warnings: ~D~%" errors failures warnings))
