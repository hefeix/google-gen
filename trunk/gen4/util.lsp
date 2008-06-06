(defun nspaces (n)
  (coerce (make-array n :initial-element #\Space) 'string))

(defun gen-vector ()
  (make-array 0 :fill-pointer 0))

; like vector-push-extend but returns the pushed item
(defun gen-push (v vec)
  (vector-push-extend v vec)
  v)

; reader is a cons of (vector . 0)
(defun reader (vec)
  (cons vec 0))

(defun read-value (r)
  (aref (car r) (cdr r)))

(defun read-advance (r)
  (prog1
      (aref (car r) (cdr r))
    (incf (cdr r))))

(defun read-done (r)
  (= (cdr r) (fill-pointer (car r))))

(defun log-eval (code) (format t "about to eval ~S~%" code) (eval code))

