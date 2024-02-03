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

(defun prepend (x y)
  (funcall #'append y x))

(defun flatten (the-list)
  (mappend #'mklist the-list))

(defun comp (&rest fns)
  (destructuring-bind (f &rest r) (reverse fns)
    (lambda (&rest args)
      (reduce (lambda (acc f)
                (funcall f acc))
              r
              :initial-value (apply f args)))))

;; pattern match tool
;; grammar of pattern
;; pat =>          | var                   | match any one expression                          |
;;                 | constant              | match just this atom                              |
;;                 | segment-pat           | match something against a sequence                |
;;                 | single-pat            | match something against one expression            |
;;                 | (pat . pat)           | match the first and the rest                      |
;; single-pat =>   | (`?is` var predicate) | test predicate on one expression                  |
;;                 | (`?or` pat...)        | match any pattern on one expression               |
;;                 | (`?and` pat...)       | match every pattern on one expression             |
;;                 | (`?not` pat...)       | succeed if pattern(s) do not match                |
;; segment-pat =>  | ((`?` var)...)        | match zero or more expressions                    |
;;                 | ((`?+` var) ... )     | match one or more expressions                     |
;;                 | ((`??` var) ... )     | match zero or one expression                      |
;;                 | ((`?if` exp )...)     | test if exp (which may contain variables) is true |
;; var =>          | `?`chars              | a symbol starting with ?                          |
;; constant =>     | atom                  | any nonvariable atom                              |

(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
             "Indicates pat-match success, with no variables.")

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun make-binding (var val)
  (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
          nil
          bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
      fail
      new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
    fail
    (let ((new-bindings (pat-match (first patterns)
                                   input bindings)))
      (if (eq new-bindings fail)
        (match-or (rest patterns) input bindings)
        new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input
  This will never bind any variables."
  (if (match-or patterns input bindings)
    fail
    bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
      (match-variable var input bindings)
      (let ((pos (first-match-pos (first pat) input start)))
        (if (null pos)
          fail
          (let ((b2 (pat-match
                      pat (subseq input pos)
                      (match-variable var (subseq input 0 pos)
                                      bindings))))
            ;; If this match failed, try another longer one
            (if (eq b2 fail)
              (segment-match pattern input bindings (+ pos 1))
              b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
  starting at position start. If pat1 is non-constant, then just return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((<= start (length input)) start)
        (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables
  The pattern looks like ((?if code) . rest)."
  (and (progv (mapcar #'car bindings)
         (mapcar #'cdr bindings)
         (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)
(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-match-fn (x)
  "Get the segment-match function for x,
  if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x,
  if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond
    ((eq bindings fail) fail)

    ((variable-p pattern)
     (match-variable pattern input bindings))

    ((eql pattern input) bindings)

    ((segment-pattern-p pattern)
     (segment-matcher pattern input bindings))

    ((single-pattern-p pattern)
     (single-matcher pattern input bindings))

    ((and (consp pattern) (consp input))
     (pat-match (rest pattern) (rest input)
                (pat-match (first pattern) (first input) bindings)))

    (t fail)))

(defun rule-based-translator
  (input rules
         &key
         (matcher #'pat-match)
         (rule-if #'first)
         (rule-then #'rest)
         (action #'sublis))
  "Find the first rule in rules that matches input,
  and apply the action to that rule."
  (some
    #'(lambda (rule)
        (let ((result (funcall matcher (funcall rule-if rule)
                               input)))
          (if (not (eq result fail))
            (funcall action result (funcall rule-then rule)))))
    rules))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
      ((atom pat) pat)
      (t (cons (expand-pat-match-abbrev (first pat))
          (expand-pat-match-abbrev (rest pat))))))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
    (expand-pat-match-abbrev expansion)))



