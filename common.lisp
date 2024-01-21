;; chap03
(defun find-all
  (item sequence &rest keyword-args
        &key (test #'eq) test-not &allow-other-keys)
  "Find all those elements of sequence that match item according to the keywords.
  Doesn't alter sequence."
  (if test-not
    (apply #'remove item sequence :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence :test (complement test) keyword-args)))

(defun find-all-if
  (predicate sequence)
  (remove-if-not predicate sequence))

(defun set-changes (old-set new-set)
  "Find elements added and deleted between two sets."
  (let ((added   (set-difference new-set old-set :test #'equal))
        (deleted (set-difference old-set new-set :test #'equal)))
    (list :added added :deleted deleted)))

(defun member-equal (item list)
  (member item list :test #'equal))

(defvar *dbg-ids* nil "Identifiers used by dbg")
(defvar *debug-output* *debug-io*)

(defun dbg (id format-string &rest args)
  "When debugging info if (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (format *debug-output* "~%")
    (apply #'format *debug-output* format-string args)))

(defun debug* (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  (setf *dbg-ids* (if (null ids) nil
                    (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  (when (member id *dbg-ids*)
    (format *debug-output* "~%")
    (dotimes (i indent) (princ "  " t))
    (apply #'format *debug-output* format-string args)))

(defun starts-with (list x)
 "Is this a list whose first element is x?"
 (and (consp list) (eql (first list) x)))

(defun random-elt
  (choices)
  (list (elt choices (random (length choices)))))

(defun mklist (x)
  (if (listp x)
    x
    (list x)))

(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

(defun flatten (the-list)
  (mappend #'mklist the-list))

(defun comp (&rest fns)
  (destructuring-bind (f &rest r) (reverse fns)
    (lambda (&rest args)
      (reduce (lambda (acc f)
                (funcall f acc))
              r
              :initial-value (apply f args)))))
