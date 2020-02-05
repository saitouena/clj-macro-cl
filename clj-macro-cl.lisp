;; for debug
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

;; comment
(defmacro comment (&body body))

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

;; delay
(defmacro delay (x)
  (let ((cache-sym (gensym "cache"))
	(called?-sym (gensym "called?")))
    `(let ((,cache-sym nil)
	   (,called?-sym nil))
       (lambda ()
	 (if ,called?-sym
	     ,cache-sym
	     (let ((res ,x))
	       (setq ,called?-sym t)
	       (setq ,cache-sym res)
	       res))))))

(defun force (delayed)
  (funcall delayed))

(comment
  ;; playground
  (mac (delay (+ 1 2)))
  (force (delay (+ 1 2)))

  (let ((print1-and-return-3-delayed (delay (progn
					      (print 1)
					      3))))
    (force print1-and-return-3-delayed)
    (force print1-and-return-3-delayed)
    (force print1-and-return-3-delayed))

  ;; sketch
  (let ((x nil)
	(called? nil))
    (lambda ()
      (if called?
	  x
	  (let ((res expr))
	    (setq called? t)
	    (setq x res)
	    res))))
  (force (delay 1)))

;; if-not
(defmacro if-not (test then else)
  `(if ,test ,else ,then))

;; and
(defmacro my-and (&rest exprs)
  (if exprs
      (destructuring-bind (e . exprs) exprs
	(let ((tmp-sym (gensym "tmp")))
	  `(let ((,tmp-sym ,e)) ;; avoid evaluating twice
	     (if ,tmp-sym
		 (my-and ,@exprs)
		 nil))))
      t))

(comment
  (mac (my-and t t t nil))
  ;; check
  (my-and (progn (print 1) t)
	  (progn (print 2) t))
  (my-and))

;; or
(defmacro my-or (&rest exprs)
  (if exprs
      (destructuring-bind (e . exprs) exprs
	(let ((tmp-sym (gensym "tmp")))
	  `(let ((,tmp-sym ,e)) ;; avoid evaluating twice
	     (if ,tmp-sym
		 t
		 (my-or ,@exprs)))))
      t))
