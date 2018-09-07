(in-package :clvm)

(defclass compiler ()
  ((in-file :accessor :in-file
	   :initarg :in-file)
   (out-file :accessor out-file
	    :initarg :out-file)))

(defun slurp-stream1 (stream)
  (with-output-to-string (out)
    (do ((x (read-char stream nil stream) (read-char stream nil stream)))
        ((eq x stream))
      (write-char x out))))

(defun make-compiler (in-file out-file)
  (make-instance 'compiler :in-file in-file :out-file out-file))

(defmethod llvm-compile ((c compiler))
  (let* ((header "'(begin \n")
	 (text (with-open-file (stream (:in-file c))
		 (uiop:slurp-stream-string stream)))
	 (footer ")")
	 (module-text (concatenate 'string header text footer)))
    module-text))

(defun generate-fn-header (fn)
  "(fn ")

(defun generate-fn-footer (fn)
  ")")

(defun generate-assembly (fn &optional (stream *standard-output*))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it,
  but in a column at least 8 spaces wide."
  ;; This version handles code that has been assembled into a vector
  (if (stringp stream)
      (with-open-file (the-stream stream)
	(generate-assembly fn the-stream))
      (if (not (fn-p fn))
	  (error
	   "generate-assembly expects an assembly instead it reaceived: ~a"
	   fn)
	  (progn
	    (format stream "~a" (generate-fn-header fn))
	    (fresh-line stream)
	    (prin1 (list :name (fn-name fn)) stream)
	    (fresh-line stream)
	    (prin1 (list :args (fn-args fn)) stream)
	    (fresh-line stream)
	    (prin1 (list :env (fn-env fn)) stream)
	    (fresh-line stream)
	    (dotimes (i (length (fn-code fn)))
	      (let ((instr (elt (fn-code fn) i)))
		(if (fn-p (second instr))
		    (generate-assembly (second instr) stream)
		    (progn
		      (prin1 instr stream)
		      (fresh-line stream)))))
	    (format stream "~a"  (generate-fn-footer fn))
	    (fresh-line stream)))))

(defun generate-object-assembley (fn &optional(stream *standard-output*) &key (binary-buffer (make-array 0 :adjustable t :element-type 'unsigned-byte)))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it,
  but in a column at least 8 spaces wide."
  ;; This version handles code that has been assembled into a vector
  (if (stringp stream)
      (with-open-file (the-stream stream)
	(generate-assembly fn the-stream))
      (if (not (fn-p fn))
	  (error
	   "generate-assembly expects an assembly instead it reaceived: ~a"
	   fn)
	  (progn
	    (format stream "~a" (generate-fn-header fn))
	    (fresh-line stream)
	    (prin1 (list :name (fn-name fn)) stream)
	    (fresh-line stream)
	    (prin1 (list :args (fn-args fn)) stream)
	    (fresh-line stream)
	    (prin1 (list :env (fn-env fn)) stream)
	    (fresh-line stream)
	    (dotimes (i (length (fn-code fn)))
	      (let ((instr (elt (fn-code fn) i)))
		(if (fn-p (second instr))
		    (generate-assembly (second instr) stream)
		    (progn
		      (prin1 instr stream)
		      (fresh-line stream)))))
	    (format stream "~a"  (generate-fn-footer fn))
	    (fresh-line stream)))))

(defun test-make-compiler ()
  (llvm-compile (make-compiler
		 #P"/home/rett/dev/llvm/llvm-lab/hello-world.clvm"
		 #P"/home/rett/dev/llvm/llvm-lab/hello-world.ll")))

;; Is this working?
(defun test-generate-assembly ()
  (with-open-file (the-stream
		 #P"/home/rett/dev/llvm/llvm-lab/hello-world.ll"
		 :direction :output
		 :if-does-not-exist :create
		 :if-exists :overwrite)
    (generate-assembly (compiler *scheme-top-level*) the-stream)))

(defun fn-list (fn-body)
  (let ((code '())
	name args env)
    (dolist (body-element fn-body)
      (let ((rest (rest body-element)))
	(case (car body-element)
	  (:args (setf args rest))
	  (:name (setf name rest))
	  (:env (setf env rest))
	  ('my-fn (setf code (cons (list 'fn (fn-list  rest)) code)))
	  (t (setf code (cons body-element code))))))
    (make-fn :code (make-array (length code) :initial-contents (reverse code))
	     :name name
	     :args args
	     :env env)))


(defmacro my-fn (&rest body)
  `(fn-list '(,@body)))
