(in-package #:cl-user)
(defpackage #:cl-base64-system (:use #:asdf #:cl))
(in-package #:cl-base64-system)

(defsystem paip-examples
  :name "paip-examples"
  :author "Norvig"
  :maintainer "vrrm"
  :licence "unknown"
  :description
  "Code made available in Norvig's PAIP, slightly modified for modern
   Common Lisp environments."
  :components
  ((:file "package")
   (:file "encode" :depends-on ("package"))
   (:file "decode" :depends-on ("package"))
   ))


(defun requires (&rest files)
  "The arguments are files that are required to run an application."
  (mapc #'load-paip-file files))

(defvar *paip-files*
  `("auxfns" "tutor" "examples"
    "intro" "simple" "overview" "gps1" "gps" "eliza1" "eliza" "patmatch"
    "eliza-pm" "search" "gps-srch" "student" "macsyma" "macsymar" "unify"
    "prolog1" "prolog" "prologc1" "prologc2" "prologc" "prologcp"
    "clos" "krep1" "krep2" "krep" "cmacsyma" "mycin" "mycin-r" "waltz"
    "othello" "othello2" "syntax1" "syntax2" "syntax3" "unifgram"
    "grammar" "lexicon" "interp1" "interp2" "interp3"
    "compile1" "compile2" "compile3" "compopt"))

(defparameter *paip-directory*
  (make-pathname :name nil :type nil
		 :defaults (or (and (boundp '*load-truename*) *load-truename*)
			       (truename ""))) ;;??? Maybe Change this
  "The location of the source files for this book.  If things don't work,
  change it to reflect the location of the files on your computer.")
