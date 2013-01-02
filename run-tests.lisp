;;;; Runs all tests from "test-cases.txt"
(load "clgs.lisp")

(let ((test-cases (with-open-file (file #P"test-cases.lisp")
                    (read file))))
  (loop for test-case in test-cases do
        (handler-case
          ;; Redirect stdout from EXECUTE-GS-PROGRAM
          (with-open-stream (*standard-output*
                              (make-string-output-stream))
            (execute-gs-program (car test-case))
            (get-output-stream-string *standard-output*))
          (error (error)
                 (format t "~A raised:~% ~A~%~%" (car test-case) error)))))
