;;;; Benchmarks

(defun pi-benchmark ()
  "Generate 1000 digits of pi"
  (time (execute-gs-program "6666,-2%{2+.2/@*\\/10.3??2*+}*")))
