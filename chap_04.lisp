;; The five stages of an AI programming project are:
;;
;; 1. Describe the problem in vague terms
;; 2. Specify the problem in algorithmic terms
;; 3. Implement the problem in a programming language
;; 4. Test the program on representative examples
;; 5. Debug and analyze the resulting program, and repeat the process


;; 1 Describe
;; NOTE: Means-end analysis is a problem-solving strategy where you start with the end goal
;; you want to achieve and then work backward to figure out the steps or means to reach that goal

;; 2 Specification
;; spec out what is the goal, the starting state and allowable operators / actions
;; GPS will then be a funciton of 3 arguments
;;   (GPS '(unknown poor) '(rich famous) list-of-ops)

;; 3 Implementation

(load "common.lisp")

(defvar *state* nil "The current state: a list of conditions.")

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation"
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (achieve-all goals) 'solved))

(defun achieve-all
  (goals)
  "Try to achieve each goal, then make sure they still hold after all the actions are executed."
  (and (every #'achieve goals)
       (subsetp goals *state*)))

(defun achieve (goal)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (or (member goal *state*)
      (some #'apply-op
            (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

(defun apply-op (op)
  "Print a message and update *state* if op is applicable."
  (when (every #'achieve (op-preconds op))
    (print (list 'executing (op-action op)))
    (let ((current-state (copy-list *state*)))
      (setf *state* (set-difference *state* (op-del-list op)))
      (setf *state* (union *state* (op-add-list op)))
      (print (set-changes current-state *state*)))
    t))
;; 4 Test

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
             :del-list '(have-money))))

(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school)
     *school-ops*)
;; => SOLVED

(GPS '(son-at-home) '(son-at-school)
     *school-ops*)
;; => NIL

;; bug: the goal specify that we will need to have money at the eod
;; so the goal is unachivable, tho GPS still mark it as solved
(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(have-money son-at-school)
     *school-ops*)
;; => NIL

(GPS '(son-at-home car-needs-battery have-money)
     '(son-at-school)
     *school-ops*)


;; Debugging the recursive subgoal problem

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "When debugging info if (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

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

(defun executing-p (x)
  "Is x of the form: (executing ...) ?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

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

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: achieve all goals using *ops*."
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

(defun achieve-all (state goals goal-stack)
  "Achieve eal goal, and make sure the still hold at the end"
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
      current-state)))

(defun achieve (state goal goal-stack)
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

(defun member-equal (item list)
  (member item list :test #'equal))

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

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add-list."
  (member-equal goal (op-add-list op)))

(defun use (oplist)
  "Use oplist as the default list operators."
  (length (setf *ops* oplist)))

(defun GPS (state goals &optional (ops *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (let ((old-ops *ops*))
    (setf *ops* ops)
    (let ((result (remove-if #'atom (achieve-all
                                      (cons'(start) state)
                                      goals nil))))
      (setf *ops* old-ops)
      result)))

(push (make-op :action 'ask-phone-number
               :preconds '(in-communication-with-shop)
               :add-list '(know-phone-number))
      *school-ops*)

(use *school-ops*)

(GPS '(son-at-home car-needs-battery have-money have-phone-book) '(son-at-school))

(debug* :gps)

(gps '(son-at-home) '(son-at-home))

(defun GPS (state goals &optional (*ops* *ops*))
  (find-all-if #'action-p
               (achieve-all (cons '(start) state) goals nil)))

(defun action-p (x)
  "Is x somethign that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))

(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))


(use (make-block-ops '(a b c)))

(GPS '((a on b)
       (b on c)
       (c on table)
       (space on a)
       (space on table))
     '((b on a)
       (c on b)))
;; => ((START) (EXECUTING (MOVE A FROM B TO TABLE)) (EXECUTING (MOVE B FROM C TO A))
;;     (EXECUTING (MOVE C FROM TABLE TO B)))
;; this works but if you change the order of the goal, it's no longer able to find a solution

(GPS '((a on b)
       (b on c)
       (c on table)
       (space on a)
       (space on table))
     '((c on b)
       (b on a)))
;; => NIL

(defun achieve-all (state goals goal-stack)
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (orderings goals)))

(defun achieve-each (state goals goal-stack)
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
      current-state)))

(defun orderings (l)
  (if (> (length l) 1)
    (list l (reverse l))
    (list l)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (appropriate-ops goal state)))))

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators,
  sorted by the number of unfulfilled preconditions."
  (sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
        :key #'(lambda (op)
                 (count-if #'(lambda (precond)
                               (not (member-equal precond state)))
                           (op-preconds op)))))

(GPS '((c on a)
       (a on table)
       (b on table)
       (space on c)
       (space on b)
       (space on table))
     '((c on table) (a on b)))

;; Ex 4.2:

(defun permute
  (l)
  (if (null l)
    '(())
    (mapcan #'(lambda (e)
                (mapcar #'(lambda (p) (cons e p))
                        (permute (remove e l :count 1 :test #'eq))))
            l)))
