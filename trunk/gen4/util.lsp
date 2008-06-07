(defun nspaces (n) (make-string n :initial-element #\Space))

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

(defun split-by-lines (s)
  (when (or (= (length s) 0) (not (eq (aref s (- (length s) 1)) #\Newline)))
    (setf s (concatenate 'string s (make-string 1 :initial-element #\Newline))))
  (let ((ret (gen-vector)) (output (make-string-output-stream)))
    (loop for c across s do 
	 (if (eq c #\Newline) 
	     (vector-push-extend (get-output-stream-string output) ret)
	     (write-char c output))
	 )
    ret)
  )

; print function must take an object and return a string
; children function must take an object and return a sequence of children
(defun tree-print 
    (destination object print-function children-function &optional 
     (horiz-space 2)  
     (vert-space 1)
     (first-line-prefix "")
     (additional-line-prefix ""))
  (let ((lines (split-by-lines (funcall print-function object)))
	(children (coerce (funcall children-function object) 'vector)))
    (dotimes  (i (length lines))
      (write-string (if (= i 0) first-line-prefix additional-line-prefix)
		    destination)
      (write-string (make-string horiz-space :initial-element 
				 (if (= i 0) #\- #\Space)) destination)
      (write-string (aref lines i) destination)
      (format destination "~%"))
    
    (let ((child-prefix-with-bar 
	   (concatenate 'string additional-line-prefix 
			(make-string horiz-space :initial-element #\Space) "|"))
	  (child-prefix-with-space
	   (concatenate 'string additional-line-prefix 
			(make-string (+ 1 horiz-space) :initial-element #\Space))))
      (dotimes (i (length children))
	(dotimes (j vert-space)
	  (write-string child-prefix-with-bar destination)
	  (format destination "~%"))
	(tree-print destination (aref children i) print-function
		    children-function horiz-space vert-space
		    child-prefix-with-bar 
		    (if (= (+ i 1) (length children))
			child-prefix-with-space
			child-prefix-with-bar))))))

(defun object-to-string (object) (format nil "~S" object))

;(tree-print t '(a (b c) d e) 'object-to-string #'(lambda (x) (if (listp x) x nil)))

  
  

