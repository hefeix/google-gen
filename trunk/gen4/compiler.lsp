(defun top-level-gen-compile (code parent) nil)

; base class for static forms
(defclass sform ()
  (
   parent
   children
   code
   )
)

(defmethod gen-compile ((f sform) code)
  (print "sform-compile")
  (setf (slot-value f 'code) code)
)

(defmethod gen-compile-children ((f sform) codelist)
  (print "sform-compile-children")
  (setf (slot-value f 'children) 
	(map 'list #'(lambda (code) (funcall top-level-gen-compile code f)) codelist)
	)
  )

; e.g. (f 3 4)
(defclass call-sform (sform)
  (
   function-symbol
   )
)

(defmethod gen-compile ((f call-sform) code)
  (call-next-method)
  (print "call-sform-compile")
  (setf (slot-value f 'function-symbol) (car code))
  (gen-compile-children f (cdr code))
  f
)

; e.g 8
(defclass constant-sform (sform)
  (
   value
   )
)

(defmethod gen-compile ((f constant-sform) code)
  (call-next-method)
  (print "constant-sform-compile")
  (setf (slot-value f 'value) code)
  f
)

; assume code is a list for now
(defun toplevel-gen-compile (code parent)
  (let ((f nil))
    (cond ((listp code)
	   (setf f (make-instance 'call-sform))
	   (gen-compile f code)
	   )
	  (t	 
	   (setf f (make-instance 'constant-sform))
	   (gen-compile f code)
	   )
	)
    (setf (slot-value f 'parent) parent)
    f
    )
)


(toplevel-gen-compile '(+ 1 2) nil)