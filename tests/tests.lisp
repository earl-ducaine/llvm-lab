(in-package :clvm)


(defparameter *hello-world-file*
  (make-pathname
   :directory (append
	       (pathname-directory (asdf:system-source-directory :llvm-lab))
	       '("tests"))
   :name "hello-world"
   :type "clvm"))


(defun run-tests ()
  (load-clvm *hello-world-file*))
