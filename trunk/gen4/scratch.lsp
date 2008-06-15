(load "program.lsp")
(new-program)
(gen-eval '(+ 2 3) :debug? t :logging? nil)

(gen-eval '(defun f (x y) (+ x y)) :debug? t)
(gen-eval (f 3 4) :debug? t)
(gen-eval '(f 6 7))
;(gen-run '(eval '(+ 2 3)) t)

(gen-eval '(defun fibo (x) (if (< x 2) 1 (+ (fibo (- x 1)) (fibo (- x 2))))) :logging? nil)

(time (gen-eval '(fibo 31) :logging? t :enform? nil :time-limit 1.0))
(time (gen-eval '(fibo 35) :step-limit 900000000 :logging? nil :time-limit 5.0))
(sb-sys:get-system-info)
;(gen-run *fiboprog* t)
(+ 1 1)
;(setf *gaussprog* '(progn (defun gauss (x) (if (= x 0) 0 (+ x (gauss (- x 1))))) (gauss 10000)))
;(gen-run *gaussprog* t)
;(time (eval *gaussprog*)) 
;(time (gen-run *gaussprog*))

;(gen-run '(let ((x 8)) (setf x 10) (+ 1 x)) t)

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

; (time (gen-run '(let ((sum 0)) (dotimes (x 30000) (setf sum (+ sum x))) sum) nil))

;(gen-debug '(dotimes (x 3) (print x)))

;(dotimes (x 10) (print x))

;(defun overlord ()
;  (handler-case (gauss 10 0)
;    (condition () -2))) 

(defun gauss (x sum)
  (if (= x 3)
      (error (make-condition 'condition))
      nil)
  (if (= x 0)
      sum
      (prog2
	  (format t "started ~S~%" x)
	  (gauss (- x 1) (+ x sum))
	(format t "finished ~S~%" x))))
	 
      
  
