(+ 1 1)

(* 2 2)

(define (add-stuff x y)
    (+ x y))

(write (add-stuff 1 2))


(defun cl-add-stuff (x y)
  (+ x y))


(defvar my-var '?my-variable)

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))


(write "(variable-p my-var)")
(write (variable-p))


(write (cl-add-stuff 3 4))
