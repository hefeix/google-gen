(defun new-vector () (make-array 5 :fill-pointer 0 :adjustable t))

(defstruct choosevector
  (sum 0.0)
  (v (new-vector)) ; vector of keys
  )

(defstruct choosetable
  ; key -> (value . position in vector)
  (key-val (make-hash-table))
  ; ceiling(log(value)) -> choosevector
  (vectors (make-hash-table)) 
  )

(defun choosetable-get (key table) 
  (gethash key (choosetable-key-val table)))

(defun choosetable-find-vector (value table)
  (let ((look (gethash (ceiling (log value)) (choosetable-vectors table))))
    (if look
	look
	(setf (gethash (ceiling (log value)) (choosetable-vectors table))
	      (make-choosevector)))))

(defun choosetable-remove (key table)
  (let ((p (choosetable-get key table)))
    (when p
      (let ((value (car p))
	    (position (cdr p)))
	(let* ((cv (choosetable-find-vector value table))
	       (v (choosevector-v cv)))
	  (setf (cdr (gethash (setf (aref v position)
				    (vector-pop v)) 
			      (choosetable-key-val table))) position)
	  (remhash key (choosetable-key-val table))
	  (incf (choosevector-sum cv) (- value))
	  (when (= 0 (fill-pointer v))
	    (remhash (ceiling (log value)) (choosetable-vectors))))))))
	
	  

(defun choosetable-set (key value table)
  (choosetable-remove key table)
  (let ((cv (choosetable-find-vector value table)))
    (incf (choosevector-sum cv) value)
    (let ((position (fill-pointer (choosevector-v cv))))
      (vector-push-extend key (choosevector-v cv))
      (setf (gethash key (choosetable-key-val table)) (cons value position)))))


  
(setf table (make-choosetable))
(choosetable-set 'a 4 table)
(choosetable-set 'b 1 table)
(choosetable-set 'c 6 table)
(print (choosetable-get 'a table))
(print (choosetable-get 'b table))
(print (choosetable-get 'c table))
