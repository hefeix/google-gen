; load utilities
(load "util.lsp")

;;; PROGRAM ;;;

(defclass program () 
  (
   (id-to-sform :initform (gen-vector))
   (log-message-type :initform (gen-vector))
   (log-message :initform (gen-vector))
   (enform-pointer :initform 0)
   (log-position-to-dform :initform (gen-vector))

   ; sforms and dforms created with the eval-loop
   (toplevel-sforms :initform (gen-vector))
   (toplevel-dforms :initform (gen-vector))
   
   )
  )

(load "form.lsp")

(defparameter *program* nil)
(defun new-program () (setf *program* (make-instance 'program)))

; time and step tracking
(defparameter *step-counter* 0)
(defparameter *step-limit* 0)
(defparameter *check-interval* 1000)
(defparameter *time-limit* 0)

(defparameter *logging?* t)

(defmethod log-position ((p program)) 
  (fill-pointer (slot-value p 'log-message-type)))

(defmethod log-entry ((p program) sform-id) 
  (when *logging?*
    (vector-push-extend 'entry (slot-value p 'log-message-type))
    (vector-push-extend sform-id (slot-value p 'log-message))))

(defmethod log-return ((p program) value)
  (when *logging?*
    (vector-push-extend 'return (slot-value p 'log-message-type))
    (vector-push-extend value (slot-value p 'log-message)))
  value)

(defun link-sforms-to-dynamic-parents ()
  (loop for sf across (slot-value *program* 'id-to-sform) do
       (when (integerp (slot-value sf 'parent))
	 (let ((df (aref (slot-value *program* 'log-position-to-dform) 
			 (slot-value sf 'parent))))
	   (setf (slot-value df 'sform-child) sf)
	   (setf (slot-value sf 'parent) df)))))
	 

(defun print-sform-trees (&optional (first-sform-id 0)) 
  (let ((v (slot-value *program* 'id-to-sform)))
    (loop for id from first-sform-id below (fill-pointer v) do
	 (let ((sf (aref v id)))
	   (when (not (typep (slot-value sf 'parent) 'sform))
	     (gen-print sf))))))
(new-program)

(defun gen-eval (code &key
		 step-limit
		 time-limit 
		 (logging? t)
		 (enform? t)
		 debug?
		 print-sforms?
		 print-rewritten-code?
		 print-dforms?
		 )
  (when debug?
    (setf print-sforms? t)
    (setf print-rewritten-code? t)
    (setf print-dforms? t))
  (when (null logging?) (setf enform? nil))
  (print code)
  (setf *logging?* logging?)
  (setf *step-limit* step-limit)
  (setf *step-counter* 0)
  (if (null time-limit) 
      (setf *time-limit* nil)
      (setf *time-limit* (+ (get-time) (* time-limit 1000000) )))
  (let ((start-time (get-time))
	(final-value nil)
	(sf (gen-push (toplevel-gen-compile code nil)
		      (slot-value *program* 'toplevel-sforms))))
    (when print-sforms? 
      (print "TOPLEVEL SFORM:")
      (gen-print sf))
    (let ((rewritten-code (rewrite sf))
	  (sform-id-before-dynamic 
	   (fill-pointer (slot-value *program* 'id-to-sform))))
      (when print-rewritten-code? 
	(format t "REWRITEN CODE: ~S~%" rewritten-code))
      (handler-case 
	  (progn 
	    (setf final-value (eval rewritten-code))
	    (when enform? 
	      (toplevel-enform)
	      (when print-dforms?	  
		(print "TOPLEVEL DFORM: ")	    
		(gen-print (vector-last 
			    (slot-value *program* 'toplevel-dforms))))
	      (link-sforms-to-dynamic-parents) )
	    (when print-sforms?
	      (print "DYNAMICALLY CREATED SFORMS: ")
	      (print-sform-trees sform-id-before-dynamic))
	    (format t "FINAL RESULT: ~S~%" final-value)
	    )
	(too-long-condition (c) (format t "caught a condition ~S~%" c) nil))
      )	  
    (format t "steps: ~S  time: ~S~%"
	    *step-counter* 
	    (* 1e-6 (- (get-time) start-time)))  
     final-value))



