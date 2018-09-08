(asdf:defsystem :llvm-lab
    :depends-on (uiop cl-fad)
    :components
    ((:file "packages")
     (:file "auxfns")
     (:file "environment")
     (:file "environment-test")
     (:file "interp1")
     (:file "interp2")
     (:file "interp3")
     (:file "compile1")
     (:file "compile2")
     (:file "compile3")
     (:file "machine")
     (:file "compiler")
     (:file "eliza1")
     (:file "eliza")
     (:file "tests/tests")
     (:file "macsyma")
     (:file "macsymar")


;;     (:file "pprint")
     ))
