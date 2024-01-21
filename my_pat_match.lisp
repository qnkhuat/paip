(load "common.lisp")

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

(defvar fail nil)
(defvar no-bindings '((t . t)))

(defun variable-p (x)
  (and (symbolp x)
       (eql #\? (char (symbol-name x) 0))))

(defun make-binding (pattern input bindings)
  (cons (cons pattern input) (if (equal bindings no-bindings)
                               '()
                               bindings)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun variable-matcher (var value bindings)
  (if (null (get-binding var bindings))
    (make-binding var value bindings)
    ;; if the variable already exists, we need to fail out
    fail))

(defun single-pattern-p (x)
  (and (consp x)
       (symbolp (car x))
       (eql #\? (char (symbol-name (car x)) 0))))


(defun match-is (pattern input bindings)
  (destructuring-bind (_ var pred) pattern
    (if (funcall (symbol-function pred) input)
      (make-binding var input bindings)
      fail)))

(setf (get '?is 'single-match) #'match-is)

(defun single-pattern-matcher (pattern input bindings)
  (funcall (get (first pattern) 'single-match) pattern input bindings))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond
    ((eql bindings fail) fail)

    ((variable-p pattern)
     (variable-matcher pattern input bindings))

    ;; constant should just keep go on
    ((eql pattern input)
     bindings)

    ((single-pattern-p pattern)
     (single-pattern-matcher pattern input bindings))

    ((and (consp pattern) (consp input))
      (pat-match (rest pattern) (rest input) (pat-match (first pattern) (first input) bindings)))

    (t fail)))

(untrace single-pattern-matcher)
(untrace pat-match)
(untrace match-is)

;; simple case
(pat-match '(i need a ?X) '(i need a vacation))
;; => ((?X . VACATION))


;; single patterns
(pat-match '(?x (?or < = >) ?y) '(3 < 4))
;; => ((?Y 4) (?x 3))

(pat-match '(x = (?is ?n numberp)) '(x = 34))
;; => ((?N 34))

(pat-match '(x = (?and (?is ?n numberp) (?is ?n oddp))) '(x = 3))


(pat-match '(a (?* ?x) d) '(a b c d))
;; => ((?X B C))

(pat-match '(a (?* ?x) (?* ?y) d) '(a b c d))
;; => ((?Y B C) (?X))

;; => ((?N 3))
