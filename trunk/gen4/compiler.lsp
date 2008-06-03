; load utilities
(load "util.lsp")

; our brilliant logging scheme
(defparameter *runlog* (gen-vector))

; array class for static forms
(defclass sform ()
  (
   (parent :initform nil)
   (children :initform nil)
   (code :initform nil)
   )
  )

; base class for dynamic forms
(defclass dform ()
  (
   (s-parent :initform nil)
   (enter-time :initform nil)
   (children :initform nil)
   (d-parent :initform nil)
   (exit-time :initform nil)
   )
  )

(defmethod rewrite ((f sform) logv)
  (let* ((rewritten-children 
	  (map 'list 
	       #'(lambda (sf) (rewrite sf logv))
	       (slot-value f 'children))))
    `(progn
       (gen-log ,f ,logv)
       (gen-log (,(car (slot-value f 'code)) ,@rewritten-children) ,logv)
       )
    )
  )

(defmethod gen-print ((f sform) indent)
  (format t (nspaces indent))
  (format t "~S~%" (slot-value f 'code))
  (map 'list #'(lambda (cf) (gen-print cf (+ 2 indent))) (slot-value f 'children))
  nil
  )

(defmethod gen-compile ((f sform) code)
  (print "sform-compile")
  (setf (slot-value f 'code) code)
  )

(defmethod gen-compile-children ((f sform) codelist)
  (print "sform-compile-children")
  (print codelist)
  (setf (slot-value f 'children) 
	(map 'list #'(lambda (code) (toplevel-gen-compile code f)) codelist)
	)
  )

; e.g. (f 3 4)
(defclass call-sform (sform)
  (
   function-symbol
   )
  )

(defclass call-dform (dform)
  (
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

(defclass constant-dform (dform)
  (
   )
  )

(defmethod rewrite ((f constant-sform) logv)
  `(progn
     (gen-log ,f ,logv)
     (gen-log ,(slot-value f 'value) ,logv)
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
	   )
	  (t	 
	   (setf f (make-instance 'constant-sform))
	   )
	)
    (gen-compile f code)
    (setf (slot-value f 'code) code)
    (setf (slot-value f 'parent) parent)
    f
    )
)

(defun toplevel-make-dform (readfrom)
  
  )

(setf *runlog* (gen-vector))
(let ((tl (toplevel-gen-compile '(+ 1 1) nil)))
  (format t "~%")
  (gen-print tl 0)
  (print (rewrite tl '*runlog*))
  (print (eval (rewrite tl '*runlog*)))
)