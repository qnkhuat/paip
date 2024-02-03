(load "common.lisp")

(defun fib (n)
  (if (<= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 4)

(defun memo (f)
  (let ((table (make-hash-table :test 'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val found-p)
          (gethash args table)
          (if found-p
              val
              (setf (gethash args table) (apply f args)))))))

(setf (fdefinition 'memo-fib) (memo #'fib))

(trace fib)

(memo-fib 4)

(defun memoize
  (fn-name)
  (setf (symbol-function fn-name) (memo (symbol-function fn-name))))

(memoize 'fib)

(fib 20)

(defmacro defun-memo (fn args &body body)
  `(memoize (defun ,fn ,args . , body)))

(defun-memo fib (n)
  (if (<= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2)))))
