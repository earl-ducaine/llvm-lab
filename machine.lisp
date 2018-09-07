(in-package :clvm)

;; Machine state

(defvar code nil)
(defvar pc 0)
(defvar env nil)
(defvar stack nil)
(defvar n-args 0)
(defvar instr nil)
(defvar breakpoint-fn nil)
(defvar *breakpoint-counter* nil)
(defvar *current-function*)
(defvar *current-breakpoint-index* 0)

(defun reset-run-state (f)
  (setf *current-function* f)
  (setf code (fn-code f))
  (setf pc 0)
  (setf env nil)
  (setf stack nil)
  (setf n-args 0)
  (setf instr nil))

(defparameter *state-log* (make-array 0
                                      :adjustable t
                                      :fill-pointer 0))




(defun record-state (f)
  (let ((log-entry  (list :current-function *current-function*
				 :code code
				 :pc pc
				 :env env
				 :stack stack
				 :n-args n-args
				 :instr instr
				 :f f)))
    (vector-push-extend log-entry *state-log*)))


(defun print-current-function (current-function)
  (format t ""))

(defun print-code (code)
  )

(defun print-pc (pc)
  )

(defun print-env (env)
  )

(defun print-stack (stack)
  )

(defun print-n-args (n-args)
  )

(defun print-instr (instr)
  )

(defun print-f (f)
  )


(defun print-state-record (state-record level)
  (destructuring-bind
	(&key current-function code pc env stack n-args instr f)
       (elt *state-log* 4)
    current-function))





(defun machine-step (&optional (n 1))
  (machine nil :breakpoint-counter (+ *current-breakpoint-index* n))
  (print-state))


(defun machine (&optional f &key breakpoint-counter state-logging)
  "Run the abstract machine on the code for f."
  (if (and *breakpoint-counter*
	   (null breakpoint-counter))
      (setf breakpoint-counter *breakpoint-counter*))
  (if f
      (reset-run-state f)
      (if *current-function*
	  (setf f *current-function*)))
  (loop
     (when state-logging
       (record-state f))
     (when breakpoint-counter
       (if (eq breakpoint-counter *current-breakpoint-index*)
	   (RETURN (top stack)))
       (format t "~%breakpoint-counter: ~s, iteration: ~s~%"
	       breakpoint-counter *current-breakpoint-index*))
     (incf *current-breakpoint-index*)
     (setf instr (elt code pc))
     (incf pc)
     (case (opcode instr)
       ;; Variable/stack manipulation instructions:
       (LVAR   (push (elt (elt env (arg1 instr)) (arg2 instr))
		     stack))
       (LSET   (setf (elt (elt env (arg1 instr)) (arg2 instr))
		     (top stack)))
       (GVAR   (push (get-global-var (arg1 instr)) stack))
       (GFVAR  (push (get-global-var (arg1 instr)) stack))
       (GSET   (set-global-var (arg1 instr) (top stack)))
       (GFSET  (set-global-var (arg1 instr) (top stack)))
       (POP    (pop stack))
       (CONST  (push (arg1 instr) stack))
       ;; Branching instructions:
       (JUMP   (setf pc (arg1 instr)))
       (FJUMP  (if (null (pop stack)) (setf pc (arg1 instr))))
       (TJUMP  (if (pop stack) (setf pc (arg1 instr))))
       ;; Function call/return instructions:
       (SAVE   (push (make-ret-addr :pc (arg1 instr)
				    :fn f :env env)
		     stack))
       (RETURN ;; return value is top of stack; ret-addr is second
	 (setf f (ret-addr-fn (second stack))
	       code (fn-code f)
	       env (ret-addr-env (second stack))
	       pc (ret-addr-pc (second stack)))
	 ;; Get rid of the ret-addr, but keep the value
	 (setf stack (cons (first stack) (rest2 stack))))
       (CALLJ  (pop env)                 ; discard the top frame
	       (setf f  (pop stack)
		     code (fn-code f)
		     env (fn-env f)
		     pc 0
		     n-args (arg1 instr)))
       (ARGS   (assert (= n-args (arg1 instr)) ()
		       "Wrong number of arguments:~
                         ~d expected, ~d supplied"
		       (arg1 instr) n-args)
	       (push (make-array (arg1 instr)) env)
	       (loop for i from (- n-args 1) downto 0 do
		    (setf (elt (first env) i) (pop stack))))
       (ARGS.  (assert (>= n-args (arg1 instr)) ()
		       "Wrong number of arguments:~
                         ~d or more expected, ~d supplied"
		       (arg1 instr) n-args)
	       (push (make-array (+ 1 (arg1 instr))) env)
	       (loop repeat (- n-args (arg1 instr)) do
		    (push (pop stack) (elt (first env) (arg1 instr))))
	       (loop for i from (- (arg1 instr) 1) downto 0 do
		    (setf (elt (first env) i) (pop stack))))
       (FN     (push (make-fn :code (fn-code (arg1 instr))
			      :env env) stack))
       (PRIM   (push (apply (arg1 instr)
			    (loop with args = nil repeat n-args
			       do (push (pop stack) args)
			       finally (return args)))
		     stack))
       ;; Continuation instructions:
       (SET-CC (setf stack (top stack)))
       (CC     (push (make-fn
		      :env (list (vector stack))
		      :code '((ARGS 1) (LVAR 1 0 ";" stack) (SET-CC)
			      (LVAR 0 0) (RETURN)))
		     stack))
       ;; Nullary operations:
       ((SCHEME-READ NEWLINE) ; *** fix, gat, 11/9/92
	(push (funcall (opcode instr)) stack))
       ;; Unary operations:
       ((CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM cl-internal-load)
	(push (funcall (opcode instr) (pop stack)) stack))
       ;; Binary operations:
       ((+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)
	(setf stack (cons (funcall (opcode instr) (second stack)
				   (first stack))
			  (rest2 stack))))
       ;; Ternary operations:
       (LIST3
	(setf stack (cons (funcall (opcode instr) (third stack)
				   (second stack) (first stack))
			  (rest3 stack))))
       ;; Constants:
       ((T NIL -1 0 1 2)
	(push (opcode instr) stack))
       ;; Other:
       ((HALT) (RETURN (top stack)))
       (otherwise (error "Unknown opcode: ~a" instr)))))

(defun init-scheme-comp ()
  "Initialize values (including call/cc) for the Scheme compiler."
  (set-global-var 'name! #'name!)
  (set-global-var 'exit
		   (new-fn :name 'exit :args '(val) :code '((HALT))))
  (set-global-var 'call/cc
		   (new-fn :name 'call/cc :args '(f)
			   :code '((ARGS 1) (CC) (LVAR 0 0 ";" f)
				   (CALLJ 1)))) ; *** Bug fix, gat, 11/9/92
  (dolist (prim *primitive-fns*)
    (set-global-var (prim-symbol prim)
		    (new-fn :env nil :name (prim-symbol prim)
			    :code (seq (gen 'PRIM (prim-symbol prim))
				       (gen 'RETURN))))))


;; (defparameter scheme-top-level
;;   '(begin (define (scheme)
;; 	   (newline)
;; 	   (display "=> ")
;; 	   (write ((compiler (read))))
;; 	   (scheme))
;;     (scheme)))

(defparameter scheme-top-level
  '(begin (define (scheme)
	   (newline)
	   (display "=> ")
	   (write ((compiler (list '+ 1 2))))
	   (scheme))
    (scheme)))

(defparameter load-clvm-wrapper
  '(begin (define (wrapper)
	   (newline)
	   (write ((compiler (read)))))
    (wrapper)))

(defparameter scheme-load-file
  '(begin (define (scheme)
	   (newline)
	   (write ((compiler (read)))))
    (scheme)))

(defun scheme (&key state-logging)
  "A compiled Scheme read-eval-print loop"
  (init-scheme-comp)
  (machine
   (compiler scheme-top-level)
   :state-logging state-logging))

(defun internal-eval (exp &key state-logging)
  "Compile and execute the expression."
  (machine
   (compiler `(exit ,exp))
   :state-logging state-logging))

(defun load-clvm-to-scheme-sexpr2 (file)
  (let ((*readtable* *scheme-readtable*))
    (with-open-file (stream file :direction :input)
      (scheme-read stream))))

(defun load-clvm-to-scheme-sexpr (file)
  (let ((*readtable* *scheme-readtable*)
	(expression ''(+ 1 1)))
    `(begin (define (wrapper)
	     (newline)
	     (write ((compiler ,expression))))
    (wrapper))))

(defun load-clvm (file)
  (init-scheme-comp)
  (machine (compiler (load-clvm-to-scheme-sexpr file))))

(defparameter scheme-top-level
  '(begin (define (scheme)
	   (newline)
	   (display "=> ")
	   (write ((compiler (read))))
	   (scheme))
    (scheme)))


(with-input-from-string (input "(exit)")
	(let ((*standard-input* input))
	  (scheme)))


(defparameter *scheme-top-level*
  '(begin (define (scheme)
	   (newline)
	   (display "=> ")
	   (write ((compiler (read))))
	   (scheme))
    (scheme)))

(defun scheme (&key state-logging)
  "A compiled Scheme read-eval-print loop"
  (init-scheme-comp)
  (machine (compiler *scheme-top-level*)))

(defun scheme-load (file &key state-logging)
  (let ((scheme-string (etypecase file
			 (string
			  (with-open-file (file-stream file :direction :input)
			    (asdf::slurp-stream-string file-stream)))
			 (stream
			  (asdf::slurp-stream-string file)))))
    (run-scheme scheme-string :state-logging state-logging)))

(defun run-scheme-load ()
  (scheme-load "~/dev/llvm-lab/llvm-lab/tests/hello-world.clvm" :state-logging t))

(defun run-scheme (scheme-text &key state-logging)
  (with-input-from-string (input scheme-text)
    (let ((*standard-input* input))
      (scheme :state-logging state-logging))))

;;; (read stream)))))


;; (defun load-cllv (file)
;;   (asdf::slurp-stream-string file)
