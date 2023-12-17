(defun mappend (fn l)
  (if (null l)
    nil
    (append (funcall fn (first l))
            (mappend fn (rest l)))))
