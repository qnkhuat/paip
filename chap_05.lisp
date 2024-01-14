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
