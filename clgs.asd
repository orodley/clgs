(asdf:defsystem :clgs
  :description "Common Lisp Golfscript interpreter"
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "clgs"
                :depends-on ("package"))
               (:file "builtins"
                :depends-on ("clgs"))))
