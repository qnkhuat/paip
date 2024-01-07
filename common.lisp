(defun mappend (fn l)
  (if (null l)
    nil
    (append (funcall fn (first l))
            (mappend fn (rest l)))))

;; chap03
(defun find-all
  (item sequence &rest keyword-args
        &key (test #'eq) test-not &allow-other-keys)
  "Find all those elements of sequence that match item according to the keywords.
  Doesn't alter sequence."
  (if test-not
    (apply #'remove item sequence :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence :test (complement test) keyword-args)))


(defun set-changes (old-set new-set)
  "Find elements added and deleted between two sets."
  (let ((added (set-difference new-set old-set :test #'equal))
        (deleted (set-difference old-set new-set :test #'equal)))
    (list :added added :deleted deleted)))
