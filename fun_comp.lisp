(load "common.lisp")

(defun square (x) (* x x))
(defun dbl (x) (* x 2))

;; my first dummy version
(defun comp
  (&rest fns)
  (lambda (&rest args)
    (let ((result args))
      (dolist (fn (reverse fns))
        (setf result (if (listp results)
                       (apply #'funcall fn result)
                       (funcall fn result))))
      result)))



(funcall (comp #'square #'dbl) 3)

(square (+ 1 2))
;; => 36

;; proper version using recursive
(defun comp (&rest fns)
  (labels ((compose-rec
             (fns args)
             (if fns
               (compose-rec (rest fns) (if (listp args)
                                         (apply (first fns) args)
                                         (funcall (first fns) args)))
               args)))
    (lambda (&rest args)
      (compose-rec (reverse fns) args))))

(funcall (comp #'square #'dbl) 3)
;; => 36

;; using reduce
(defun comp (&rest fns)
  (destructuring-bind (f &rest r) (reverse fns)
    (lambda (&rest args)
      (reduce (lambda (acc f)
                (funcall f acc))
              (rest (reverse fns))
              :initial-value (apply (first (reverse fns)) args)))))

(funcall (comp #'square #'dbl) 3)
;; => 36

(defmacro comp-macro (&rest fns)
  `(destructuring-bind (f &rest r) (reverse ',fns)
     (lambda (&rest args)
       (reduce (lambda (acc fn)
                 (funcall fn acc))
               r
               :initial-value (apply f args)))))

(funcall (comp-macro square dbl) 3)
