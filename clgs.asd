(asdf:defsystem :clgs
  :description "Common Lisp Golfscript interpreter"
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "gs-object")
               (:file "clgs"      :depends-on ("package" "gs-object"))
               (:file "builtins"  :depends-on ("clgs" "package" "gs-object"))
               (:file "run-tests" :depends-on ("clgs" "builtins" "package"))))
