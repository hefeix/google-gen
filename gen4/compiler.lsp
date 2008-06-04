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
   (return-value :initform nil)

   (s-parent :initform nil)
   (children :initform (gen-vector))
   (d-parent :initform nil)

   ; time at which your static form was logged
   (entry-time :initform nil)
   ; time at which your return value was logged
   (exit-time :initform nil)
   )
  )

(defmethod rewrite ((f sform) logv)
  (let* ((rewritten-children 
	  (map 'list 
	       #'(lambda (sf) (rewrite sf logv))
	       (slot-value f 'children))))
    `(progn
       (gen-push ,f ,logv)
       (gen-push (,(car (slot-value f 'code)) ,@rewritten-children) ,logv)
       )
    )
  )

(defmethod gen-print ((f sform) indent)
  (format t (nspaces indent))
  (format t "~S~%" (slot-value f 'code))
  (map 'list #'(lambda (cf) (gen-print cf (+ 2 indent))) (slot-value f 'children))
  nil
  )

(defmethod gen-print ((f dform) indent)
  (format t (nspaces indent))
  (format t "return-value:~S [~S..~S] ~S~%" 
	  (slot-value f 'return-value)
	  (slot-value f 'entry-time)
	  (slot-value f 'exit-time)
	  (slot-value (slot-value f 's-parent) 'code)
	  )
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
     (gen-push ,f ,logv)
     (gen-push ,(slot-value f 'value) ,logv)
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

; pass in a read iterator
(defun toplevel-enform (ri)
  (let ((sf (read-advance ri)))
    (enform sf ri)
    )
)
 
(defmethod enform ((f sform) ri)
  (let (
	(df (create-dform f))
	(next (read-advance ri))
	)
    (setf (slot-value df 's-parent) f)
    (setf (slot-value df 'entry-time) (- (cdr ri) 2))
	  
    ; getting dforms for children
    (loop while (typep next 'sform) do
	 (let ((child (enform next ri)))
	   (gen-push child (slot-value df 'children))
	   (setf (slot-value child 'd-parent) df)
	   )
	 (setf next (read-advance ri))
	 )
    ; this is now our return value
    (setf (slot-value df 'return-value) next)
    (setf (slot-value df 'exit-time) (- (cdr ri) 1))
    df
    )
)
   
; creating dforms
(defmethod create-dform ((f call-sform))
  (make-instance 'call-dform)
)
(defmethod create-dform ((f constant-sform))
  (make-instance 'constant-dform)
)

(setf *runlog* (gen-vector))
(let ((tl (toplevel-gen-compile '(+ 1 (+ 31 2)) nil)))
  (format t "~%")
  (gen-print tl 0)
  (print (rewrite tl '*runlog*))
  (print (eval (rewrite tl '*runlog*)))
  (gen-print (toplevel-enform (reader *runlog*)) 0)
)