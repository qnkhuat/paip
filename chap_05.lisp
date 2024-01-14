(load "common.lisp")

(defun simple-equal (x y)
  (if (or (atom x) (atom y))
    (eql x y)
    (and (simple-equal (first x) (first y))
         (simple-equal (rest x) (rest y)))))

(defun pat-match (pattern input)
  (if (variable-p pattern)
    T
    (if (or (atom pattern) (atom input))
      (eql pattern input)
      (and (pat-match (first pattern) (first input))
           (pat-match (rest pattern) (rest input))))))

(defun variable-p (x)
  "x is a variable if it's a symbol starts with '?'"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(pat-match '(I need a ?X) ' (I need a vacation))
;; => T

(pat-match '(I need a ?X) ' (I need a vacation so bad))
;; => NIL

(sublis '((?X . vacation))
        '(what would it mean to you if you got a ?X huh ?))

;; we want pat-match now to return a cons list of variable and word so that we could do subsitution

(defconstant fail nil "Indicates pat-match failure")
(defconstant no-bindings '((t . t)) "matches success with no variables")

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val) (if (eq bindings no-bindings)
                         nil
                         bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (dbg :pat "Match var. var: ~a, Input: ~a, bindings: ~a" var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond
      ;; extend the binding if this binding does not exist
      ((not binding) (extend-bindings var input bindings))
      ;; if the binding exists and the value of the bindng is the same, we are safe to subsitute
      ((equal input (binding-val binding)) bindings)
      ;; this is the case where a binding has 2 values, which doesn't make sense so we'' fail
      (t fail))))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail)
         fail)

        ((variable-p pattern)
         (match-variable pattern input bindings))

        ((eql pattern input)
         bindings)

        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings)))

        (t
          fail)))

(match-variable '?x '(1 2) '((?x 1)))


(pat-match '(i need a ?X) '(i need a vacation))
;; => ((?X . VACATION))

(sublis (pat-match '(i need a ?X) '(i need a vacation)) '(what would it mean to you if you got a ?X ?))
;; => (WHAT WOULD IT MEAN TO YOU IF YOU GOT A VACATION ?)

;; We now want to make the variable to be able to match a sequence of word, not just single word;
;; to do that we provide the ?* variable name pattern for such cases

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
 (dbg :pat "Pat match. pattern: ~a input: ~a bindings: ~a" pattern input bindings)
 (cond ((eq bindings fail)
        fail)

       ((variable-p pattern)
        (match-variable pattern input bindings))

       ((eql pattern input)
        bindings)

       ;; input here is a list
       ((segment-pattern-p pattern)
        (segment-match pattern input bindings))

       ((and (consp pattern) (consp input))
        (pat-match (rest pattern) (rest input)
                   (pat-match (first pattern) (first input) bindings)))

       (t
         fail)))

(defun segment-pattern-p (pattern)
  "Is this a segment match pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun segment-match
  (pattern input bindings &optional (start 0))
  (dbg :pat "Segment match: ~a, Input: ~a" pattern input)
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
   (if (null pat)
     ;; if there is no more pattern, this matches the rest of the input
     (match-variable var input bindings)
     ;; we assume that pat starts with a constant
     ;; In other words, a pattern can't have 2 consecutive vars
     (let ((pos (position (first pat) input :start start :test #'equal)))
       (if (null pos)
         fail
         (let ((b2 (pat-match pat (subseq input pos) bindings)))
           ;; if this match failed, try another longer one
           ;; If it worked, check that the variables match
           (if (eq b2 fail)
             (segment-match pattern input bindings (+ pos 1))
             (match-variable var (subseq input 0 pos) b2))))))))

(undebug :pat)

(pat-match '((?* ?p) need (?* ?x))
           '(Mr Hu lot and I need a vacation))
;; => ((?P MR HU LOT AND I) (?X A VACATION))

(pat-match '((?* ?x) is a (?* ?y)) '(what he is is a fool))
;; => ((?X WHAT HE IS) (?Y FOOL))

;; doesn't work for this case tho
(pat-match '((? *?x) a b (? *?x)) '(1 2 a b a b 1 2 a b))
;; => NIL

(defun segment-match
  (pattern input bindings &optional (start 0))
  (dbg-indent :pat start "Segment match. pattern: ~a, Input: ~a" pattern input)
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
   (if (null pat)
     ;; if there is no more pattern, this matches the rest of the input
     (match-variable var input bindings)
     ;; we assume that pat starts with a constant
     ;; In other words, a pattern can't have 2 consecutive vars
     (let ((pos (position (first pat) input :start start :test #'equal)))
       (if (null pos)
         fail
         (let ((b2 (pat-match pat (subseq input pos)
                              (match-variable var (subseq input 0 pos) bindings))))
           ;; if this match failed, try another longer one
           ;; If it worked, check that the variables match
           (if (eq b2 fail)
             (segment-match pattern input bindings (+ pos 1))
             b2)))))))

(untrace segment-match)
(untrace pat-match)
(untrace match-variable)


(pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b))
;; => ((?X 1 2 A B))

;; ELIZA

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do.  Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))
     (Were you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(sublis (switch-viewpoint (pat-match (rule-pattern (first rule)))))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am are))
          words))

(defun use-eliza-rules (input)
  ;;(loop for rule in *eliza-rules*
  ;;      for replacement = (pat-match (rule-pattern rule) input)
  ;;      unless (null replacement)
  ;;      return (sublis replacement (random-elt (rule-responses rule))))
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                (sublis (switch-viewpoint result)
                        (random-elt (rule-responses rule))))))
        *eliza-rules*))

(loop for x in '(1  1 3 4)
      for y = (+ x 10)
      unless (= y 13)
      return (+ y 10))

(when T
  (print "SUP"))

(defun eliza ()
  (loop
    (print 'eliza>)
    (write (flatten (use-eliza-rules (read))) :pretty t)))
