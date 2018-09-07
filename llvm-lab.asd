(asdf:defsystem :llvm-lab
    :around-compile "hemlock-internals::build-documentation"
    :depends-on (:clx :chanl)
    :components
    ((:file "compile1.lisp")))
