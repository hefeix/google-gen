; load utilities
(load "util.lsp")

; our brilliant logging scheme
(defparameter *runlog* (gen-vector))

; class for static forms
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
   ; return value for your form
   (return-value :initform nil)

   ; pointer to the sform that you executed
   (s-parent :initform nil)
   ; pointer back to the parent dform
   (d-parent :initform nil)
   ; pointer to all dforms corresponding to static children
   ; also points to a function entry dform on function calls
   (children :initform (gen-vector))
 
   ; time at which your static form was logged
   (entry-time :initform nil)
   ; time at which your return value was logged
   (exit-time :initform nil)
   )
  )

; rewrite code to log form entry, and form exit return value
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

; print sforms
(defmethod gen-print ((f sform) indent)
  (format t (nspaces indent))
  (format t "~S~%" (slot-value f 'code))
  (map 'list #'(lambda (cf) (gen-print cf (+ 3 indent))) (slot-value f 'children))
  nil
  )

; print dforms
(defmethod gen-print ((f dform) indent)
  (format t (nspaces indent))
  (format t "return-value:~S [~S..~S] ~S~%" 
	  (slot-value f 'return-value)
	  (slot-value f 'entry-time)
	  (slot-value f 'exit-time)
	  (slot-value (slot-value f 's-parent) 'code)
	  )
  (map 'list #'(lambda (cf) (gen-print cf (+ 3 indent))) (slot-value f 'children))
  nil
  )

; compiling code into sforms
; this call is called from all more specific calls
(defmethod gen-compile ((f sform) code)
  (print "sform-compile")
  (setf (slot-value f 'code) code)
  )

; compile your children and set them
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

; creating parallel dforms from sforms
(defmethod create-dform ((f call-sform))
  (make-instance 'call-dform)
)

; compiling a specific call-sform
(defmethod gen-compile ((f call-sform) code)
  (call-next-method)
  (print "call-sform-compile")
  (setf (slot-value f 'function-symbol) (car code))
  (gen-compile-children f (cdr code))
  f
)

; (let ( (var code) (var code) ) code code code)

(defclass let-sform (sform)
  (
   variables
   )
  )

(defclass let-dform (dform)
  (
   )
  )

; compiling a specific constant sform
(defmethod gen-compile ((f let-sform) code)
  (call-next-method)
  (print "let-sform-compile")
  (let ((let-variable-assignments (car (cdr code))))
    (setf (slot-value f 'variables)
	  (map 'list #'car let-variable-assignments))
    (gen-compile-children 
     f
     (concatenate 'list
		  (map 'list #'cadr let-variable-assignments)
		  (cddr code))
     )
    )
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

; constant-forms are rewritten slightly differently
(defmethod rewrite ((f constant-sform) logv)
  `(progn
     (gen-push ,f ,logv)
     (gen-push ,(slot-value f 'value) ,logv)
     )
  )

; compiling a specific constant sform
(defmethod gen-compile ((f constant-sform) code)
  (call-next-method)
  (print "constant-sform-compile")
  (setf (slot-value f 'value) code)
  f
)

(defmethod create-dform ((f constant-sform))
  (make-instance 'constant-dform)
)


; assume code is a list for now
; this is the top level function called to compile code, returns an sform
(defun toplevel-gen-compile (code parent)
  (let ((f nil))
    (if (typep code 'atom)
	(setf f (make-instance 'constant-sform))
	(let ((first (car code)))
	  (cond 
	    ((eq first 'let) (setf f (make-instance 'let-sform)))
	    (t (setf f (make-instance 'call-sform)))
	    )
	  )
	)
    (gen-compile f code)
    (setf (slot-value f 'code) code)
    (setf (slot-value f 'parent) parent)
    f
    )
)

; pass in a read iterator for a vector that is a run log
; makes the dform tree for that run and returns the top level dform
(defun toplevel-enform (ri)
  (let ((sf (read-advance ri)))
    (enform sf ri)
    )
)

; the basics are simple. everything logged is either an sform
; in which case it's an entry, or a return value. A simple
; recursive algorithm matches everything up 
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
   
(setf *runlog* (gen-vector))
(let ((tl (toplevel-gen-compile 
	   '(let ((x 1)) (+ 1 (+ 31 2)))
	   nil)))
  (format t "~%")
  (gen-print tl 0)
  ;(print (rewrite tl '*runlog*))
  ;(print (eval (rewrite tl '*runlog*)))
  ;(gen-print (toplevel-enform (reader *runlog*)) 0)
)