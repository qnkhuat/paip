(load "common.lisp")

(defstruct op "An operation"
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(defvar *ops* nil "A list of available operators.")

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add-list."
  (member-equal goal (op-add-list op)))

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators,
  sorted by the number of unfulfilled preconditions."
  (sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
        :key #'(lambda (op)
                 (count-if #'(lambda (precond)
                               (not (member-equal precond state)))
                           (op-preconds op)))))

(defun orderings (l)
  (if (> (length l) 1)
    (list l (reverse l))
    (list l)))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable"
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state* (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state*)
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state*)
              (op-add-list op)))))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (appropriate-ops goal state)))))

(defun achieve-each (state goals goal-stack)
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
      current-state)))

(defun achieve-all (state goals goal-stack)
  "Try achiving the goals but shuffle the goals a bit to ensure the order of the goal is not important."
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (orderings goals)))

(defun executing-p (x)
  "Is x of the form: (executing ...) ?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun action-p (x)
  "Is x somethign that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))

(defun GPS (state goals &optional (*ops* *ops*))
  (find-all-if #'action-p
               (achieve-all (cons '(start) state) goals nil)))

(defun use (oplist)
  "Use oplist as the default list operators."
  (length (setf *ops* oplist)))

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun debug* (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  (setf *dbg-ids* (if (null ids) nil
                    (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defparameter *school-ops*
  (list
    (make-op :action 'drive-son-to-school
             :preconds '(son-at-home car-works)
             :add-list '(son-at-school)
             :del-list '(son-at-home))
    (make-op :action 'shop-installs-battery
             :preconds '(car-needs-battery shop-knows-problem shop-has-money)
             :add-list '(car-works))
    (make-op :action 'tell-shop-problem
             :preconds '(in-communication-with-shop)
             :add-list '(shop-knows-problem))
    (make-op :action 'telephone-shop
             :preconds '(know-phone-number)
             :add-list '(in-communication-with-shop))
    (make-op :action 'look-up-number
             :preconds '(have-phone-book)
             :add-list '(know-phone-number))
    (make-op :action 'give-shop-money
             :preconds '(have-money)
             :add-list '(shop-has-money)
             :del-list '(have-money))
    (make-op :action 'ask-phone-number
             :preconds '(in-communication-with-shop)
             :add-list '(know-phone-number))))

(use *school-ops*)

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
    (make-op :action action :preconds preconds
             :add-list add-list :del-list del-list)))

;; update the ops with new convention
(mapc #'convert-op *school-ops*)


(debug* :gps)

(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))


;; Ex 4.3 begins

(defparameter *eat-dessert-ops*
  (list
    (make-op :action 'eat-ice-cream
             :preconds '(has-ice-cream)
             :add-list '(joy-dessert)
             :del-list '(has-ice-cream))
    (make-op :action 'eat-cake
             :preconds '(has-cake)
             :add-list '(joy-dessert)
             :del-list '(has-cake))
    (make-op :action 'buy-cake
             :preconds '(has-money)
             :add-list '(has-cake)
             :del-list '(has-money))
    (make-op :action 'eat-cake-when-buy
             :preconds '(has-cake)
             :add-list '(joy-dessert has-ice-cream)
             :del-list '(has-cake))))

(mapc #'convert-op *eat-dessert-ops*)
(use *eat-dessert-ops*)

(debug* :gps)

(GPS '(has-money)
     '(joy-dessert))

;; => ((START) (EXECUTING BUY-CAKE) (EXECUTING EAT-CAKE-WHEN-BUY)
;;     (EXECUTING EAT-ICE-CREAM))

;; SOLUTION
(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (first (sort (mapcar #'(lambda (op) (apply-op state goal op goal-stack))
                                (appropriate-ops goal state))
                        #'<
                        :key #'length)))))

(GPS '(has-money)
     '(joy-dessert))
;; => ((START) (EXECUTING BUY-CAKE) (EXECUTING EAT-CAKE))
