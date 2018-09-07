;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File interp1.lisp: simple Scheme interpreter, including macros.

(in-package :clvm)

(defun set-var! (var val env)
  "Set a variable to a value, in the given or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
      (set-global-var var val))
  val)

(defun get-var (var env)
  "Get the value of a variable, from the given or global environment."
    (if (assoc var env)
        (second (assoc var env))
        (get-global-var var)))

(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  (nconc (mapcar #'list vars vals) env))

(defparameter *scheme-procs*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (null? null) (eq? eq) (equal? equal) (eqv? eql)
    (write prin1) (display princ) (newline terpri)))

(defun init-scheme-interp ()
  "Initialize the scheme interpreter with some global variables."
  ;; Define Scheme procedures as CL functions:
  (mapc #'init-scheme-proc *scheme-procs*)
  ;; Define the boolean `constants'. Unfortunately, this won't
  ;; stop someone from saying: (set! t nil)
  (set-global-var t t)
  (set-global-var nil nil))

(defun init-scheme-proc (f)
  "Define a Scheme procedure as a corresponding CL function."
  (if (listp f)
      (set-global-var (first f) (symbol-function (second f)))
      (set-global-var f (symbol-function f))))

;; (defun scheme (&optional x)
;;   "A Scheme read-eval-print loop (using interp)"
;;   ;; Modified by norvig Jun 11 96 to handle optional argument
;;   ;; instead of always going into a loop.
;;   (init-scheme-interp)
;;   (if x
;;       (interp x nil)
;;     (loop (format t "~&==> ")
;;       (print (interp (read) nil)))))

;;;; The following version handles macros:
(defun interp (x &optional env)
  "Interpret (evaluate) the expression x in the environment env.
  This version handles macros."
  (cond
    ((symbolp x) (get-var x env))
    ((atom x) x)
    ((scheme-macro (first x))              ;***
     (interp (scheme-macro-expand x) env)) ;***
    ((case (first x)
       (QUOTE  (second x))
       (BEGIN  (last1 (mapcar #'(lambda (y) (interp y env))
                              (rest x))))
       (SET!   (set-var! (second x) (interp (third x) env) env))
       (IF     (if (interp (second x) env)
                   (interp (third x) env)
                   (interp (fourth x) env)))
       (LAMBDA (let ((parms (second x))
                     (code (maybe-add 'begin (rest2 x))))
                 #'(lambda (&rest args)
                     (interp code (extend-env parms args env)))))
       (t      ;; a procedure application
               (apply (interp (first x) env)
                      (mapcar #'(lambda (v) (interp v env))
                              (rest x))))))))

(defun scheme-macro (symbol)
  (and (symbolp symbol) (get symbol 'scheme-macro)))

(defmacro def-scheme-macro (name parmlist &body body)
  "Define a Scheme macro."
  `(setf (get ',name 'scheme-macro)
         #'(lambda ,parmlist .,body)))

(defun scheme-macro-expand (x)
  "Macro-expand this Scheme expression."
  (if (and (listp x) (scheme-macro (first x)))
      (scheme-macro-expand
        (apply (scheme-macro (first x)) (rest x)))
      x))

(def-scheme-macro let (bindings &rest body)
  `((lambda ,(mapcar #'first bindings) . ,body)
    .,(mapcar #'second bindings)))

(def-scheme-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin .,body)
      `(let (,(first bindings))
         (let* ,(rest bindings) . ,body))))

(def-scheme-macro and (&rest args)
  (cond ((null args) 'T)
        ((length=1 args) (first args))
        (t `(if ,(first args)
                (and . ,(rest args))))))

(def-scheme-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(let ((,var ,(first args)))
                (if ,var ,var (or . ,(rest args))))))))

(def-scheme-macro cond (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (first clauses))
         `(or ,(first clauses) (cond .,(rest clauses))))
        ((starts-with (first clauses) 'else)
         `(begin .,(rest (first clauses))))
        (t `(if ,(first (first clauses))
                (begin .,(rest (first clauses)))
                (cond .,(rest clauses))))))

(def-scheme-macro case (key &rest clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(mapcar
                #'(lambda (clause)
                    (if (starts-with clause 'else)
                        clause
                        `((member ,key-val ',(first clause))
                          .,(rest clause))))
                clauses)))))


;; scheme-macro
;;  (defmacro funcall (f . args)
;;    `((progn ,f) ,@args))

;; (apply + '(1 2 3))
(def-scheme-macro funcall (name &rest args)
   `(,name ,@args))

;;(apply
(def-scheme-macro define (name &rest body)
  (if (atom name)
      `(begin (set! ,name . ,body) ',name)
      `(define ,(first name)
         (lambda ,(rest name) . ,body))))

(def-scheme-macro defun (name args &rest body)
  `(define name
         (lambda ,args . ,body)))

;; from ecl
;; (defmacro clvm-defun (&whole whole name vl &body body &environment env &aux doc-string)
;;   ;; Documentation in help.lsp
;;   (multiple-value-setq (body doc-string) (clvm-remove-documentation body))
;;   (let* ((function `(define ,(firstext::lambda-block ,name ,vl ,@body))
;;          (global-function `#'(ext::lambda-block ,name ,vl
;;                                                 (declare (si::c-global))
;;                                                 ,@body)))
;;     (when *dump-defun-definitions*
;;       (print function)
;;       (setq function `(si::bc-disassemble ,function)))
;;     `(progn
;;        ,(ext:register-with-pde whole `(si::fset ',name ,global-function))
;;        ,@(si::expand-set-documentation name 'function doc-string)
;;        ,(let ((hook *defun-inline-hook*))
;;           (and hook (funcall hook name global-function env)))
;;        ',name))))

;; @(defun si::process-declarations (body &optional doc)
;;   cl_object documentation = ECL_NIL, declarations = ECL_NIL, specials = ECL_NIL;
;; @
;;   for (; !Null(body); body = ECL_CONS_CDR(body)) {
;;     cl_object form;
;;     unlikely_if (!ECL_LISTP(body))
;;       FEill_formed_input();
;;     form = ECL_CONS_CAR(body);
;;     if (!Null(doc) && ecl_stringp(form) && !Null(ECL_CONS_CDR(body))) {
;;       if (documentation != ECL_NIL)
;;         break;
;;       documentation = form;
;;       continue;
;;     }
;;     if (ECL_ATOM(form) || (ECL_CONS_CAR(form) != @'declare')) {
;;       break;
;;     }
;;     for (form = ECL_CONS_CDR(form); !Null(form); ) {
;;       cl_object sentence = pop(&form);
;;       declarations = ecl_cons(sentence, declarations);
;;       if (pop(&sentence) == @'special') {
;;         while (!Null(sentence)) {
;;           cl_object v = pop(&sentence);
;;           assert_type_symbol(v);
;;           specials = ecl_cons(v, specials);
;;         }
;;       }
;;     }
;;   }
;;   @(return cl_nreverse(declarations) body documentation specials);
;; @)



;; Some permutations
;;
;; docstring befor declaration
;; docstring after declaration
;; docstring in the middle of declaration
;;
;; multiple doc strings befor declaration -- error
;; multiple doc strings in the midst of declarations -- error
;;
;; one doc string in the midst of declarations one docstring after
;; declarations final docstring is form


(defparameter defun-expression
  '(defun init-scheme-interp (value)
    (declare (ignore value))
    "Initialize the scheme interpreter with some global variables."
    (mapc #'init-scheme-proc *scheme-procs*)
    (set-global-var t t)
    (set-global-var nil nil)))

(defparameter forms (cdddr defun-expression))

(defun declarep (form)
  (and (listp form)
       (eq (car form) 'declare)))

;; Get everything from the the front of the list that might be a
;; docstring or a declaration
(defun maybe-docstrings-declarations-rest (forms)
  (do (maybe-docstrings-declarations
       (form (pop forms) (pop forms)))
      ((and (not (declarep form))
	    (not (stringp form)))
       (when form
	 (push form forms))
       (values (reverse maybe-docstrings-declarations) forms))
    (push form maybe-docstrings-declarations)))


(defun clvm-process-declarations (body &optional doc)
  (declare (ignore doc))
  (multiple-value-bind (maybe-forms rest)
      (maybe-docstrings-declarations-rest (cdddr body))
    (do (docstring
	 declarations
	 last-string-forms
	 (maybe-form (pop maybe-forms) (pop maybe-forms)))
	((null maybe-form)
	 (values (mapcar (lambda (declaration)
			   (cdr declaration))
			 (reverse declarations))
		 (concatenate
		  'list
		  (reverse last-string-forms)
		  rest)
		 docstring))
      (cond ((and (not docstring)
		  (stringp maybe-form))
	     (setf docstring maybe-form))
	    ((declarep maybe-form)
	     (push maybe-form declarations))
	    ((and docstring
		  (stringp maybe-form)
		  (or (null maybe-forms)
		      (stringp (car maybe-forms))))
	     (push maybe-form last-string-forms))
	    (t
	     (error "something malformed"))))))

(defun clvm-remove-documentation (body)
  (multiple-value-bind (decls body doc)
      (clvm-process-declarations body t)
    (when decls (push `(declare ,@decls) body))
    (values body doc)))










(def-scheme-macro defvar (name &rest body)
  (if (atom name)
      `(begin (set! ,name . ,body) ',name)
      (error "defvar needs to be an atom: ~a" name)))

(def-scheme-macro defun (name args &rest body)
  `(define ,name
       (lambda ,args . ,body)))

(def-scheme-macro delay (computation)
  `(lambda () ,computation))

(def-scheme-macro letrec (bindings &rest body)
  `(let ,(mapcar #'(lambda (v) (list (first v) nil)) bindings)
     ,@(mapcar #'(lambda (v) `(set! .,v)) bindings)
     .,body))

;;; Expand scheme macro.
(defun expand-scheme-macro (macro-symbol macro-expression)
  (apply (get macro-symbol 'scheme-macro)
	 macro-expression))

(apply (get 'define 'scheme-macro) '(define (add-stuff x y)
    (+ x y)))
