;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File compile3.lisp: Scheme compiler with assembler
;;;; and peephole clvm-optimizer.  Also the abstract machine simulator.
;;;; After loading this file, load the clvm-optimizers in compopt.lisp.

;;; Bug fixes by Erann Gat, gat@aig.Jpl.Nasa.Gov, November 1992




(in-package :clvm)

(defun opcode (instr) (if (label-p instr) :label (first instr)))
(defun args (instr) (if (listp instr) (rest instr)))
(defun arg1 (instr) (if (listp instr) (second instr)))
(defun arg2 (instr) (if (listp instr) (third instr)))
(defun arg3 (instr) (if (listp instr) (fourth instr)))

(defsetf arg1 (instr) (val) `(setf (second ,instr) ,val))

;;; ==============================

(defun assemble (fn)
  "Turn a list of instructions into a vector."
  (multiple-value-bind (length labels)
      (asm-first-pass (fn-code fn))
    (setf (fn-code fn)
          (asm-second-pass (fn-code fn)
                           length labels))
    fn))

(defun asm-first-pass (code)
  "Return the labels and the total code length."
  (let ((length 0)
        (labels nil))
    (dolist (instr code)
      (if (label-p instr)
          (push (cons instr length) labels)
          (incf length)))
    (values length labels)))

(defun asm-second-pass (code length labels)
  "Put code into code-vector, adjusting for labels."
  (let ((addr 0)
        (code-vector (make-array length)))
    (dolist (instr code)
      (unless (label-p instr)
        (if (is instr '(JUMP TJUMP FJUMP SAVE))
            (setf (arg1 instr)
                  (cdr (assoc (arg1 instr) labels))))
        (setf (aref code-vector addr) instr)
        (incf addr)))
    code-vector))

;;; ==============================

(defun show-fn (fn &optional (stream *standard-output*) (indent 2))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it,
  but in a column at least 8 spaces wide."
  ;; This version handles code that has been assembled into a vector
  (if (not (fn-p fn))
      (format stream "~8a" fn)
      (progn
        (fresh-line)
        (dotimes (i (length (fn-code fn)))
          (let ((instr (elt (fn-code fn) i)))
            (if (label-p instr)
                (format stream "~a:" instr)
                (progn
                  (format stream "~VT~2d: " indent i)
                  (dolist (arg instr)
                    (show-fn arg stream (+ indent 8)))
                  (fresh-line))))))))

(defstruct ret-addr fn pc env)

(defun is (instr op)
  "True if instr's opcode is OP, or one of OP when OP is a list."
  (if (listp op)
      (member (opcode instr) op)
      (eq (opcode instr) op)))

(defun top (stack) (first stack))

(defun init-scheme-comp ()
  "Initialize values (including call/cc) for the Scheme compiler."
  (set-global-var 'exit
    (new-fn :name 'exit :args '(val) :code '((HALT))))
  (set-global-var 'call/cc
    (new-fn :name 'call/cc :args '(f)
            :code '((ARGS 1) (CC) (LVAR 0 0 ";" f)
		    (CALLJ 1)))) ; *** Bug fix, gat, 11/9/92
  (dolist (prim *primitive-fns*)
     (setf (get (prim-symbol prim) 'global-val)
           (new-fn :env nil :name (prim-symbol prim)
                   :code (seq (gen 'PRIM (prim-symbol prim))
                              (gen 'RETURN))))))

;;;; Peephole Clvm-Optimizer

(defun clvm-optimize (code)
  "Perform peephole optimization on assembly code."
  (let ((any-change nil))
    ;; Clvm-Optimize each tail
    (loop for code-tail on code do
          (setf any-change (or (clvm-optimize-1 code-tail code)
                               any-change)))
    ;; If any changes were made, call clvm-optimize again
    (if any-change
        (clvm-optimize code)
        code)))

;;; ==============================

(defun clvm-optimize-1 (code all-code)
  "Perform peephole optimization on a tail of the assembly code.
  If a change is made, return true."
  ;; Data-driven by the opcode of the first instruction
  (let* ((instr (first code))
         (clvm-optimizer (get-clvm-optimizer (opcode instr))))
    (when clvm-optimizer
      (funcall clvm-optimizer instr code all-code))))

;;; ==============================

(let ((clvm-optimizers (make-hash-table :test #'eql)))

  (defun get-clvm-optimizer (opcode)
    "Get the assembly language clvm-optimizer for this opcode."
    (gethash opcode clvm-optimizers))

  (defun put-clvm-optimizer (opcode fn)
    "Store an assembly language clvm-optimizer for this opcode."
    (setf (gethash opcode clvm-optimizers) fn)))

;;; ==============================

(defun gen1 (&rest args) "Generate a single instruction" args)
(defun target (instr code) (second (member (arg1 instr) code)))
(defun next-instr (code) (find-if (complement #'label-p) code))

;;; ==============================

(defmacro def-clvm-optimizer (opcodes args &body body)
  "Define assembly language clvm-optimizers for these opcodes."
  (assert (and (listp opcodes) (listp args) (= (length args) 3)))
  `(dolist (op ',opcodes)
     (put-clvm-optimizer op #'(lambda ,args .,body))))

;;;; Now for some additions and answers to exercises:

;;; ==============================

(defparameter eof "EoF")
(defun eof-object? (x) (eq x eof))
(defvar *scheme-readtable* (copy-readtable))

(defun scheme-read (&optional (stream *standard-input*))
  (let ((*readtable* *scheme-readtable*))
    (read stream nil eof)))

;;; ==============================

(set-dispatch-macro-character #\# #\t
  #'(lambda (&rest ignore) t)
  *scheme-readtable*)

(set-dispatch-macro-character #\# #\f
  #'(lambda (&rest ignore) nil)
  *scheme-readtable*)

(set-dispatch-macro-character #\# #\d
  ;; In both Common Lisp and Scheme, #x, #o and #b are hexidecimal,
  ;; octal, and binary, e.g. #xff = #o377 = #b11111111 = 255 In Scheme
  ;; only, #d255 is decimal 255.
  #'(lambda (stream &rest ignore)
      (let ((*read-base* 10)) (scheme-read stream)))
  *scheme-readtable*)

(set-macro-character #\`
  #'(lambda (s ignore) (list 'quasiquote (scheme-read s)))
  nil *scheme-readtable*)

(set-macro-character #\,
   #'(lambda (stream ignore)
       (let ((ch (read-char stream)))
         (if (char= ch #\@)
             (list 'unquote-splicing (read stream))
             (progn (unread-char ch stream)
                    (list 'unquote (read stream))))))
   nil *scheme-readtable*)

(defparameter *primitive-fns*
  '((+ 2 + true)
    (- 2 - true)
    (* 2 * true)
    (/ 2 / true)
    (< 2 <)
    (> 2 >)
    (<= 2 <=)
    (>= 2 >=)
    (/= 2 /=)
    (= 2 =)
    (eq? 2 eq)
    (equal? 2 equal)
    (eqv? 2 eql)
    (not 1 not)
    (null? 1 not)
    (car 1 car)
    (cdr 1 cdr)
    (cadr 1 cadr)
    (cons 2 cons true)
    (list 1 list1 true)
    (list 2 list2 true)
    (list 3 list3 true)
    (load 1 cl-internal-load nil t)
    (read 0 scheme-read nil t)
    (eof-object? 1 eof-object?)
    ;;***
    (write 1 write nil t)
    (display 1 display nil t)
    (newline 0 newline nil t)
    (compiler 1 compiler t)
    (name! 2 name! true t)
    (random 1 random true nil)))


;(setf (scheme-macro 'quasiquote) 'quasi-q)

(defun quasi-q (x)
  "Expand a quasiquote form into append, list, and cons calls."
  (cond
    ((vectorp x)
     (list 'apply 'vector (quasi-q (coerce x 'list))))
    ((atom x)
     (if (constantp x) x (list 'quote x)))
    ((starts-with x 'unquote)
     (assert (and (rest x) (null (rest2 x))))
     (second x))
    ((starts-with x 'quasiquote)
     (assert (and (rest x) (null (rest2 x))))
     (quasi-q (quasi-q (second x))))
    ((starts-with (first x) 'unquote-splicing)
     (if (null (rest x))
         (second (first x))
         (list 'append (second (first x)) (quasi-q (rest x)))))
    (t (combine-quasiquote (quasi-q (car x))
                           (quasi-q (cdr x))
                           x))))

(defun combine-quasiquote (left right x)
  "Combine left and right (car and cdr), possibly re-using x."
  (cond ((and (constantp left) (constantp right))
         (if (and (eql (eval left) (first x))
                  (eql (eval right) (rest x)))
             (list 'quote x)
             (list 'quote (cons (eval left) (eval right)))))
        ((null right) (list 'list left))
        ((starts-with right 'list)
         (list* 'list left (rest right)))
        (t (list 'cons left right))))

(defun scheme-read (&optional (stream *standard-input*))
  (let ((*readtable* *scheme-readtable*))
    (convert-numbers (read stream nil '(exit)))))

(defun my-car (the-list)
  (funcall (read-from-string "first") '(a b)))

(defun cl-internal-load (file)
  (let ((*readtable* *scheme-readtable*))
    (with-open-file (s file)
      (loop for expression = (read s nil :eof)
	 until  (eq expression :eof)
	 collect (comp-go expression)))))


(defun run-cl-internal-load ()
  (init-scheme-comp)
  (cl-internal-load "./test.lisp"))

(defun scheme-read-from-string (&optional (stream *standard-input*))
  (let ((*readtable* *scheme-readtable*))
    (convert-numbers (read stream nil eof))))

(defun convert-numbers (x)
  "Replace symbols that look like Scheme numbers with their values."
  ;; Don't copy structure, make changes in place.
  (typecase x
    (cons   (setf (car x) (convert-numbers (car x)))
            (setf (cdr x) (convert-numbers (cdr x)))
	    x) ; *** Bug fix, gat, 11/9/92
    (symbol (or (convert-number x) x))
    (vector (dotimes (i (length x))
              (setf (aref x i) (convert-numbers (aref x i))))
	    x) ; *** Bug fix, gat, 11/9/92
    (t x)))

(defun convert-number (symbol)
  "If str looks like a complex number, return the number."
  (let* ((str (symbol-name symbol))
         (pos (position-if #'sign-p str))
         (end (- (length str) 1)))
    (when (and pos (char-equal (char str end) #\i))
      (let ((re (read-from-string str nil nil :start 0 :end pos))
            (im (read-from-string str nil nil :start pos :end end)))
        (when (and (numberp re) (numberp im))
          (complex re im))))))

(defun sign-p (char) (find char "+-"))
