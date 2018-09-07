(in-package :clvm)

(defparameter *global-vars* (make-hash-table))

(defun get-global-var (var)
  (let* ((val (gethash var *global-vars*)))
    (unless val
      (error "Unbound variable: ~a" var))
    val))

(defun set-global-var (var val)
  (setf (gethash var *global-vars*) val))

(defun setf-global-var (var val)
  (setf (gethash var *global-vars*) val))

(defsetf get-global-var set-global-var)

(defun run-setf-get-global-var ()
  (setf (get-global-var 'b) 'my-b))

(defun list-global-vars ()
  (maphash (lambda (k v)
	     (format t "key: ~s, val: ~s~%" k v))
	   *global-vars*))
