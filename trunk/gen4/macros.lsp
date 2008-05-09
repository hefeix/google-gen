(defun f (x) (+ 1 x))
(defun g (y) (+ 2 y))

(defun gen (f) (rewrite-element f))

(defun rewrite-element (element)
  (if (listp element)
      (rewrite-function-form element)
      element))

(defun rewrite-element-list (elements)
  (mapcar #'rewrite-element elements))

(defun rewrite-function-form (theform)
  (let* ((name (car theform))
	 (params (cdr theform))
	 (rewritten-params (rewrite-element-list params))
	 (rv-var (gensym)))
    `(progn
       (print (list "calling" (quote ,theform)))
       (let ((,rv-var ,(cons name rewritten-params)))
	 (print (list "return" (quote ,name) ,rv-var))
	 ,rv-var))))

(defun log-let-setting (setting)
  (let ((var (car setting)))
    `(print (list "setting" (quote ,var) ,var))))

(defun rewrite-let-form (theform)
  (let* ((settings (cadr theform))
	 (params (cddr theform))
	 (new-settings (mapcar #'rewrite-element-list settings))
	 (new-params (mapcar #'rewrite-element-list params)))
    `(let ,new-settings
       ,@(mapcar #'log-let-setting settings) 
       ,@new-params)))

(defmacro gen_lambda (parameter_list &rest forms)
  `(lambda ,parameter_list
     (print (list ,@parameter_list))
     ,@forms))

(defmacro gen_defun (function_name parameter_list &rest forms)
  `(setf (symbol-function (quote ,function_name)) 
			  (gen_lambda ,parameter_list ,@forms)))



    
    