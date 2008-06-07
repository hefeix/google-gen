; load utilities
;(load "util.lsp")

;;; SFORM AND DFORM BASE CLASSES ;;;

(defparameter *id-to-sform* (gen-vector))

; class for static forms
(defclass sform ()
  (
   (parent :initform nil)
   (children :initform nil)
   (code :initform nil)
   (id :initform nil)
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

;;; LOGGING
; our brilliant logging scheme:
; We log function entry and exit in a vector.
; On function entry we log the sform
; On function exit, we log the return value, and possibly other values.
; If there are multiple values, or if the value is an sform or a list, 
; the values are placed in a list. 
(defparameter *runlog* (gen-vector))

(defun encode-value-for-log (v) 
  (if (or (listp v) (integerp  v)) (list v) v))

(defun num-log-values (v) (if (listp v) (length v) 1))

(defun get-log-value (v which) (if (listp v) (nth which v) v))

(defun gen-push-encode (v vec)
  (vector-push-extend (encode-value-for-log v) vec) v)

(defun gen-push-encode-multiple-values (v vec)
  (vector-push-extend v vec) (first v))

;;; END LOGGING


(defmethod assign-id ((f sform))
  (setf (slot-value f 'id) (fill-pointer *id-to-sform*))
  (vector-push-extend f *id-to-sform*))

(defmethod rewrite-children ((f sform) logv)
  (map 'list #'(lambda (sf) (rewrite sf logv)) (slot-value f 'children)))

(defmethod rewrite-core ((f sform) logv)
  `(,(car (slot-value f 'code)) ,@(rewrite-children f logv)))
  
; rewrite code to log form entry, and form exit return value
(defmethod rewrite ((f sform) logv)
  `(progn
     (gen-push ,(slot-value f 'id) ,logv)
     (gen-push-encode ,(rewrite-core f logv) ,logv)))

(defmethod to-string ((f sform))
  (format nil "~S id=~S~%" (slot-value f 'code) (slot-value f 'id)))

; print sforms
(defmethod gen-print ((f sform))
  (tree-print t f 'to-string (lambda (x) (slot-value x 'children))))

(defmethod to-string ((f dform))
  (format nil "return-value:~S [~S..~S] ~S~%" 
	  (slot-value f 'return-value)
	  (slot-value f 'entry-time)
	  (slot-value f 'exit-time)
	  (slot-value (slot-value f 's-parent) 'code)))

; print sforms
(defmethod gen-print ((f dform))
  (tree-print t f 'to-string (lambda (x) (slot-value x 'children))))

; compiling code into sforms
; this call is called from all more specific calls
(defmethod gen-compile ((f sform) code)
  (print "sform-compile")
  (assign-id f)
  (setf (slot-value f 'code) code))

; compile your children and set them
(defmethod gen-compile-children ((f sform) codelist)
  (print "sform-compile-children")
  (print codelist)
  (setf (slot-value f 'children) 
	(map 'list #'(lambda (code) (toplevel-gen-compile code f)) codelist)))


;;; FORM TYPES ;;;

;;; CALL ;;;
; e.g. (f 3 4)

(defclass call-sform (sform)( function-symbol))

(defclass call-dform (dform) ())

; creating parallel dforms from sforms
(defmethod create-dform ((f call-sform)) (make-instance 'call-dform))

; compiling a specific call-sform
(defmethod gen-compile ((f call-sform) code)
  (call-next-method)
  (print "call-sform-compile")
  (setf (slot-value f 'function-symbol) (car code))
  (gen-compile-children f (cdr code))
  f)


;;; LET ;;;

; (let ( (var code) (var code) ) code code code)

(defclass let-sform (sform) (variables))

(defclass let-dform (dform)())

; compiling a specific constant sform
(defmethod gen-compile ((f let-sform) code)
  (call-next-method)
  (print "let-sform-compile")
  (let ((let-variable-assignments (car (cdr code))))
    (setf (slot-value f 'variables)
	  (map 'list #'car let-variable-assignments))
    (gen-compile-children 
     f (concatenate 'list (map 'list #'cadr let-variable-assignments) 
		    (cddr code)))) f )

; rewrite code to log form entry, and form exit return value
(defmethod rewrite-core ((f let-sform) logv)
  (let* ((rewritten-children (rewrite-children f logv)))
    `(let ,(map 'list #'list (slot-value f 'variables) rewritten-children)
       ,@(nthcdr (list-length (slot-value f 'variables)) 
		 rewritten-children))))
       
(defmethod create-dform ((f let-sform)) (make-instance 'let-dform))



;;; FUNCTION-ENTRY ;;;

; the top level of a function
(defclass function-entry-sform (sform)
  (function-name
   variables
   defining-dform ))

(defclass function-entry-dform (dform) ())

(defmethod to-string ((f function-entry-sform))
  (concatenate 
   'string 
   (call-next-method)
   (format nil "defining-dform=~S~%" (slot-value f 'defining-dform))))

; pass in code = (lambda (args) body) or (function-name (args) body)
(defmethod gen-compile ((f function-entry-sform) code)
  (call-next-method)
  (print "function-entry-sform-compile")
  (setf (slot-value f 'function-name) (first code))
  (setf (slot-value f 'variables) (second code))
  (gen-compile-children f (cddr code))
  f)

(defmethod rewrite-core ((f function-entry-sform) logv)
  `(progn ,@(rewrite-children f logv)))

(defmethod create-dform ((f function-entry-sform))
  (make-instance 'function-entry-dform))


;;; DEFUN ;;;

; (defun function-name (args) body)

(defclass defun-sform (sform)
  (function-name 
   variables))

(defclass defun-dform (dform)
  (function-entry-sform-child))

(defmethod gen-compile ((f defun-sform) code)
  (call-next-method)
  (print "defun-sform-compile")
  (setf (slot-value f 'function-name) (second code))
  (setf (slot-value f 'variables) (third code))
  f)

(defmethod rewrite ((f defun-sform) logv)
  `(let ((fe (make-instance 'function-entry-sform)))
     (progn
       (gen-push ,(slot-value f 'id) ,logv)
       (gen-compile fe ',(cdr (slot-value f 'code)))       
       (gen-push-encode-multiple-values 
	(list (eval (list 'defun ',(slot-value f 'function-name) 
			  ',(slot-value f 'variables)
			  (rewrite fe ',logv))) (slot-value fe 'id)) ,logv))))

(defmethod create-dform ((f defun-sform)) (make-instance 'defun-dform))



;;; CONSTANT ;;;

; e.g 8
(defclass constant-sform (sform) ( value))

(defclass constant-dform (dform) ())

; constant-forms are rewritten slightly differently
(defmethod rewrite-core ((f constant-sform) logv)
  (slot-value f 'value))

; compiling a specific constant sform
(defmethod gen-compile ((f constant-sform) code)
  (call-next-method)
  (print "constant-sform-compile")
  (setf (slot-value f 'value) code)
  f)

(defmethod create-dform ((f constant-sform)) (make-instance 'constant-dform))

;;; VARIABLE
; e.g a
(defclass variable-sform (sform) (variable-symbol))

(defclass variable-dform (dform) ())

(defmethod rewrite-core ((f variable-sform) logv)
  (slot-value f 'variable-symbol))

; compiling a specific constant sform
(defmethod gen-compile ((f variable-sform) code)
  (call-next-method)
  (print "variable-sform-compile")
  (setf (slot-value f 'variable-symbol) code)
  f
)

(defmethod create-dform ((f variable-sform)) (make-instance 'variable-dform))



;;; SETF ;;; 

(defclass setf-sform (sform) ( variable-symbol))

(defclass setf-dform (dform)())

(defmethod gen-compile ((f setf-sform) code)
  (call-next-method)
  (print "setf-sform-compile")
  (setf (slot-value f 'variable-symbol) (second code))
  (gen-compile-children f (cddr code))
  f )

(defmethod rewrite-core ((f setf-sform) logv)
  (let ((rewritten-child (rewrite (car (slot-value f 'children)) logv)))
    `(setf ,(slot-value f 'variable-symbol) ,rewritten-child))) 
	

(defmethod create-dform ((f setf-sform)) (make-instance 'setf-dform))


;;; END FORM TYPES ;;;



; assume code is a list for now
; this is the top level function called to compile code, returns an sform
(defun toplevel-gen-compile (code parent)
  (let ((f nil))
    (cond 
      ; nils are constant
      ((null code) (setf f (make-instance 'constant-sform)))
      ; symbols are variables
      ((symbolp code) (setf f (make-instance 'variable-sform)))
      ; this leaves numbers
      ((typep code 'atom) (setf f (make-instance 'constant-sform)))
      (t
       (let ((first (car code)))
	 (cond 
	   ((eq first 'let) (setf f (make-instance 'let-sform)))
	   ((eq first 'setf) (setf f (make-instance 'setf-sform)))
	   ((eq first 'defun) (setf f (make-instance 'defun-sform)))
	   (t (setf f (make-instance 'call-sform)))
	   )
	 )
       )
      )
    (gen-compile f code)
    (setf (slot-value f 'parent) parent)
    f
    )
)
      
; pass in a read iterator for a vector that is a run log
; makes the dform tree for that run and returns the top level dform
(defun toplevel-enform (ri)
  (let ((sf (aref *id-to-sform* (read-advance ri))))
    (enform sf ri)))

; the basics are simple. everything logged is either an sform
; in which case it's an entry, or a return value. A simple
; recursive algorithm matches everything up 
(defmethod enform ((f sform) ri)
  (let ((df (create-dform f))
	(next (read-advance ri)))
    (setf (slot-value df 's-parent) f)
    (setf (slot-value df 'entry-time) (- (cdr ri) 2))
	  
    ; getting dforms for children
    (loop while (integerp next) do
	 (let ((child (enform (aref *id-to-sform* next) ri)))
	   (gen-push child (slot-value df 'children))
	   (setf (slot-value child 'd-parent) df))
	 (setf next (read-advance ri)))
    ; this is now our return value
    (setf (slot-value df 'return-value) (get-log-value next 0) )
    (when (typep df 'defun-dform)
      (setf (slot-value (aref *id-to-sform* (get-log-value next 1))
			'defining-dform) df))
    (setf (slot-value df 'exit-time) (- (cdr ri) 1))
    df))
   
(defun print-sform-trees () 
  (loop for f across *id-to-sform* do
       (when (null (slot-value f 'parent))
	 (gen-print f))))

(setf *runlog* (gen-vector))
(let ((tl (toplevel-gen-compile 
;	   '(+ 2 3)
;	   '(let ((x 8)) (setf x 10) (+ 1 x))
	   '(progn (defun g(x y) (+ x y)) (g 3 4))
	   nil)))
  (format t "~%")
  (gen-print tl)
  (format t "~S" (rewrite tl '*runlog*))
  (print (eval (rewrite tl '*runlog*)))
  (gen-print (toplevel-enform (reader *runlog*)))
  (print-sform-trees)
)


