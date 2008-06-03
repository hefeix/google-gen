(defun nspaces (n)
  (coerce (make-array n :initial-element #\Space) 'string))

(defun gen-vector ()
  (make-array 0 :fill-pointer 0)
)

(defun gen-log (v vec)
  (vector-push-extend v vec)
  v
  )

; reader is a cons of (vector . 0)
(defun reader (vec)
  (cons vec 0)
  )

(defun reader-value (r)
  (svref (car r) (cdr r))
  )

(defun reader-advance (r)
  (incf (cdr r))
)

(defun reader-done (r)
  (= (cdr r) (fill-pointer (car r)))
)