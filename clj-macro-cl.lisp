;; for debug
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

;; when
(defmacro my-when (test &body body)
  `(if ,test
       (progn
	 ,@body)))

(my-when t
  (print 1)
  (print 2))

(my-when nil
  (print 1)
  (print 2))

;; when-not
(defmacro when-not (test &body body)
  `(if ,test
       nil
       (progn
	 ,@body)))

;; cond
(defmacro my-cond (&rest clauses)
  (if clauses
      (destructuring-bind ((test . body) . clauses) clauses
	  `(if ,test
	       (progn
		 ,@body)
	       (my-cond ,@clauses)))
      nil))

(my-cond)

(my-cond
  (nil 1)
  (nil 2)
  (t 3))
