; load utilities
;(load "util.lsp")

;;; SFORM AND DFORM BASE CLASSES ;;;

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

(defmethod assign-id ((f sform))
  (setf (slot-value f 'id) (fill-pointer (slot-value *program* 'id-to-sform)))
  (vector-push-extend f (slot-value *program* 'id-to-sform)))


; always called from rewrite-core
(defmethod rewrite-children ((f sform))
  (map 'list #'(lambda (sf) (rewrite sf)) (slot-value f 'children)))

; always called from rewrite
(defmethod rewrite-core ((f sform))
  `(,(car (slot-value f 'code)) ,@(rewrite-children f)))
  
; rewrite code to log form entry, and form exit return value
(defmethod rewrite ((f sform))
  `(progn
     (log-entry ,(slot-value f 'id))
     (log-return ,(rewrite-core f))))

; non-recursive - meant to be called by tree-print
(defmethod to-string ((f sform))
  (concatenate 'string 
	       (format nil "~S id=~S~%" (slot-value f 'code) (slot-value f 'id))
	       (if (typep (slot-value f 'parent) 'sform) ""
		   (format nil "parent=~S~%" (slot-value f 'parent)))
	       )
  )

; print sforms
(defmethod gen-print ((f sform))
  (format t "~%")
  (tree-print t f 'to-string (lambda (x) (slot-value x 'children))))

; non-recursive - meant to be called by tree-print
(defmethod to-string ((f dform))
  (format nil "~S time:[~S..~S] ~S~%" 
	  (slot-value f 'return-value)
	  (slot-value f 'entry-time)
	  (slot-value f 'exit-time)
	  (slot-value (slot-value f 's-parent) 'code)))

; print sforms
(defmethod gen-print ((f dform))
  (format t "~%")
  (tree-print t f 'to-string (lambda (x) (slot-value x 'children))))

; compiling code into sforms
; this call is called from all more specific calls
(defmethod gen-compile ((f sform) code)
  (assign-id f)
  (setf (slot-value f 'code) code))

; compile your children and set them
(defmethod gen-compile-children ((f sform) codelist)
  ; (print codelist)
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
  (let ((let-variable-assignments (car (cdr code))))
    (setf (slot-value f 'variables)
	  (map 'list #'car let-variable-assignments))
    (gen-compile-children 
     f (concatenate 'list (map 'list #'cadr let-variable-assignments) 
		    (cddr code)))) f )

; rewrite code to log form entry, and form exit return value
(defmethod rewrite-core ((f let-sform))
  (let* ((rewritten-children (rewrite-children f)))
    `(let ,(map 'list #'list (slot-value f 'variables) rewritten-children)
       ,@(nthcdr (list-length (slot-value f 'variables)) 
		 rewritten-children))))
       
(defmethod create-dform ((f let-sform)) (make-instance 'let-dform))



;;; FUNCTION-ENTRY ;;;

; the top level of a function that we define
; either by lambda or by defun
; Note: a function-entry-sform is not created from toplevel compilation.
;  It is created from the dynamic execution of a rewritten defun or lambda.
(defclass function-entry-sform (sform)
  (function-name ; either the name, or 'lambda
   variables))

(defclass function-entry-dform (dform) ())

; pass in code = (lambda (args) body) or (function-name (args) body)
(defmethod gen-compile ((f function-entry-sform) code)
  (call-next-method)
  (setf (slot-value f 'function-name) (first code))
  (setf (slot-value f 'variables) (second code))
  (gen-compile-children f (cddr code))
  f)

(defmethod rewrite-core ((f function-entry-sform))
  `(progn ,@(rewrite-children f)))

(defmethod create-dform ((f function-entry-sform))
  (make-instance 'function-entry-dform))


;;; DEFUN ;;;

; (defun function-name (args) body)

(defclass defun-sform (sform)
  (function-name
   variables
   function-code))

(defclass defun-dform (dform) 
  (
   (sform-child :initform nil)
   )
)

(defmethod gen-compile ((f defun-sform) code)
  (call-next-method)
  (setf (slot-value f 'function-name) (second code))
  (setf (slot-value f 'variables) (third code))
  (setf (slot-value f 'function-code) (nthcdr 3 code)) ; not yet used
  f)

(defmethod rewrite-core ((f defun-sform))
  ; may want to make g-function-entry a gen-sym in case any other 
  ;   code uses the symbol 'fe
  `(let ((g-function-entry (make-instance 'function-entry-sform)))
     (gen-compile g-function-entry ',(cdr (slot-value f 'code)))
     (setf (slot-value g-function-entry 'parent) (- (program-time) 1))
     (eval (list 'defun 
		 ',(slot-value f 'function-name) 
		 ',(slot-value f 'variables)
		 (rewrite g-function-entry)))))


(defmethod create-dform ((f defun-sform)) (make-instance 'defun-dform))

(defmethod to-string ((f defun-dform))
  (let ((sfc (slot-value f 'sform-child)))
    (concatenate 'string (call-next-method)
		 (format nil "sform-child=~S"
			 (if sfc (slot-value sfc 'id) "none")))))



;;; EVAL ;;;

; (eval original-exp)

(defclass eval-sform (sform) ())

(defclass eval-dform (dform) 
  (
   (sform-child :initform nil)
   ))

; compiling a specific call-sform
(defmethod gen-compile ((f eval-sform) code)
  (call-next-method)
  (gen-compile-children f (cdr code))
  f)

(defmethod rewrite-core ((f eval-sform))
  `(let ((g-dform-id (- (program-time) 1)))
     (gen-eval ,@(rewrite-children f) g-dform-id)))


(defmethod create-dform ((f eval-sform)) (make-instance 'eval-dform))

(defmethod to-string ((f eval-dform))
  (let ((sfc (slot-value f 'sform-child)))
    (concatenate 'string (call-next-method)
		 (format nil "sform-child=~S"
			 (if sfc (slot-value sfc 'id) "none")))))




;;; CONSTANT ;;;

; e.g 8
(defclass constant-sform (sform) ( value))

(defclass constant-dform (dform) ())

; constant-forms are rewritten slightly differently
(defmethod rewrite-core ((f constant-sform))
  (slot-value f 'value))

; compiling a specific constant sform
(defmethod gen-compile ((f constant-sform) code)
  (call-next-method)
  (setf (slot-value f 'value) code)
  f)

(defmethod create-dform ((f constant-sform)) (make-instance 'constant-dform))

;;; QUOTE ;;;

; e.g 8
(defclass quote-sform (sform) (value))

(defclass quote-dform (dform) ())

; quote-forms are rewritten slightly differently
(defmethod rewrite-core ((f quote-sform)) 
  (list 'quote (slot-value f 'value)))

; compiling a specific quote sform
(defmethod gen-compile ((f quote-sform) code)
  (call-next-method)
  (setf (slot-value f 'value) (second code))
  f)

(defmethod create-dform ((f quote-sform)) (make-instance 'quote-dform))

;;; VARIABLE
; e.g a
(defclass variable-sform (sform) (variable-symbol))

(defclass variable-dform (dform) ())

(defmethod rewrite-core ((f variable-sform))
  (slot-value f 'variable-symbol))

; compiling a specific constant sform
(defmethod gen-compile ((f variable-sform) code)
  (call-next-method)
  (setf (slot-value f 'variable-symbol) code)
  f
)

(defmethod create-dform ((f variable-sform)) (make-instance 'variable-dform))



;;; SETF ;;; 

(defclass setf-sform (sform) ( variable-symbol))

(defclass setf-dform (dform)())

(defmethod gen-compile ((f setf-sform) code)
  (call-next-method)
  (setf (slot-value f 'variable-symbol) (second code))
  (gen-compile-children f (cddr code))
  f )

(defmethod rewrite-core ((f setf-sform))
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
	   ((eq first 'eval) (setf f (make-instance 'eval-sform)))
	   ((eq first 'quote) (setf f (make-instance 'quote-sform)))
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

;;; PROGRAM ;;;

(defclass program () 
  (
   (id-to-sform :initform (gen-vector))
   (log-message-type :initform (gen-vector))
   (log-message :initform (gen-vector))
   (time-to-dform :initform (gen-vector))
   )
  )

(defun new-program () (setf *program* (make-instance 'program)))
(new-program)

(defun program-time () (fill-pointer (slot-value *program* 'log-message-type)))

(defun log-entry (sform-id) 
  (vector-push-extend 'entry (slot-value *program* 'log-message-type))
  (vector-push-extend sform-id (slot-value *program* 'log-message)))

(defun log-return (value)
  (vector-push-extend 'return (slot-value *program* 'log-message-type))
  (vector-push-extend value (slot-value *program* 'log-message))
  value)

; evals the rewrite of compiling the code
(defun gen-eval (code &optional (parent nil))
  (eval (rewrite (toplevel-gen-compile code parent))))
      
; pass in a read iterator for a vector that is a run log
; makes the dform tree for that run and returns the top level dform
(defun toplevel-enform ()
  (let ((type-reader (reader (slot-value *program* 'log-message-type)))
	(message-reader (reader (slot-value *program* 'log-message))))
    (read-advance type-reader)
    (let ((sf (aref (slot-value *program* 'id-to-sform) 
		    (read-advance message-reader))))
      (enform sf type-reader message-reader))))

; the basics are simple. everything logged is either an sform
; in which case it's an entry, or a return value. A simple
; recursive algorithm matches everything up 
(defmethod enform ((f sform) type-reader message-reader)
  (let ((df (create-dform f))
	(next-type (read-advance type-reader))
	(next-message (read-advance message-reader)))
    (vector-push-extend df (slot-value *program* 'time-to-dform))
    (setf (slot-value df 's-parent) f)
    (setf (slot-value df 'entry-time) (- (cdr type-reader) 2))
    ;getting dforms for children
    (loop while (eq next-type 'entry) do
	 (let ((child (enform (aref (slot-value *program* 'id-to-sform) 
				    next-message) 
			      type-reader message-reader)))
	   (gen-push child (slot-value df 'children))
	   (setf (slot-value child 'd-parent) df))
	 (setf next-type (read-advance type-reader))
	 (setf next-message (read-advance message-reader)))
    ; this is now our return value
    (setf (slot-value df 'return-value) next-message)
    (setf (slot-value df 'exit-time) (- (cdr type-reader) 1))
    (vector-push-extend df (slot-value *program* 'time-to-dform))
    df))

(defun link-sforms-to-dynamic-parents ()
  (loop for sf across (slot-value *program* 'id-to-sform) do
       (when (integerp (slot-value sf 'parent))
	 (let ((df (aref (slot-value *program* 'time-to-dform) 
			 (slot-value sf 'parent))))
	   (setf (slot-value df 'sform-child) sf)
	   (setf (slot-value sf 'parent) df)))))
	 

(defun print-sform-trees () 
  (loop for sf across (slot-value *program* 'id-to-sform) do
       (when (not (typep (slot-value sf 'parent) 'sform))
	 (gen-print sf))))

(defun gen-run (code &optional (should-print nil) ) 
  (new-program)
  (let ((result (gen-eval code)))
    (when should-print 
      (let ((dform-root (toplevel-enform)))
	(link-sforms-to-dynamic-parents)
	(gen-print dform-root)
	(print-sform-trees)
	)
      )
    result))

;(gen-run '(progn (defun f (x y) (+ x y)) (f 3 4)) t)
(gen-run '(eval '(+ 2 3)) t)

;(setf *fiboprog* '(progn (defun fibo (x) (if (< x 2) 1 (+ (fibo (- x 1)) (fibo (- x 2))))) (fibo 5)))
;(setf *gaussprog* '(progn (defun gauss (x) (if (= x 0) 0 (+ x (gauss (- x 1))))) (gauss 100000)))
;(gen-run *fiboprog* t)
;(time (eval *gaussprog*)) 
;(time (gen-run *gaussprog*))

;(let ((tl (toplevel-gen-compile 
;	   '(+ 2 3)
;	   '(let ((x 8)) (setf x 10) (+ 1 x))
;	   '(progn (defun g(x y) (+ x y)) (g 3 4))
;	   '(eval '(+ 2 3))
;	   nil)))
;  (format t "~%")
;  (gen-print tl)
;  (format t "~S" (rewrite tl '*runlog*))
;  (print (eval (rewrite tl '*runlog*)))
;  (gen-print (toplevel-enform (reader *runlog*)))
;  (print-sform-trees)
;)


