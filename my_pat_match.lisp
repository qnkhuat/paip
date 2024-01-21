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

(defun get-binding (var bindings)
  (assoc var bindings))

(defun make-binding (var input bindings)
  (if (null (get-binding var bindings))
    (cons (cons var input) (if (equal bindings no-bindings)
                             '()
                             bindings))
    fail))

(defun single-match-fn (x)
  (and (symbolp x )
       (get x 'single-match)))

(defun segment-match-fn (x)
  (and (symbolp x )
       (get x 'segment-match)))

(defun variable-p (x)
  (and (symbolp x)
       (eql #\? (char (symbol-name x) 0))))

(defun single-pattern-p (x)
  (and (consp x)
       (single-match-fn (car x))))

(defun segment-pattern-p (x)
  (and (consp x)
       (consp (car x))
       (segment-match-fn (caar x))))

(defun variable-matcher (var value bindings)
  (make-binding var value bindings))

(defun match-is (pattern input bindings)
  (destructuring-bind (_ var pred) pattern
    (if (funcall (symbol-function pred) input)
      (make-binding var input bindings)
      fail)))

(setf (get '?is 'single-match) #'match-is)

(defun single-pattern-matcher (pattern input bindings)
  (funcall (single-match-fn (car pattern)) pattern input bindings))

(setf (get '? 'segment-match) #'match-?)

(defun segment-match (pattern input bindings &optional (start 0)))

(defun segment-matcher (pattern input bindings)
  (funcall (segment-match-fn (car pattern)) pattern input bindings))

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

    ((segment-pattern-p pattern)
     (segment-matcher pattern input bindings))

    ((and (consp pattern) (consp input))
      (pat-match (rest pattern) (rest input)
                 (pat-match (first pattern) (first input) bindings)))

    (t fail)))

(trace pat-match)
(trace variable-matcher)
(trace make-binding)
(trace get-binding)
(trace variable-p)
(trace single-pattern-p)
(untrace single-pattern-matcher)
(untrace match-is)

;; simple case
(pat-match '(i need a ?X) '(i need a vacation))
;; => ((?X . VACATION))

(pat-match '(?x) '(vacation) no-bindings)


;; single patterns
;(pat-match '(?x (?or < = >) ?y) '(3 < 4))
;; => ((?Y 4) (?x 3))

(pat-match '(x = (?is ?n numberp)) '(x = 34))
;; => ((?N 34))

;(pat-match '(x = (?and (?is ?n numberp) (?is ?n oddp))) '(x = 3))


;; segment matches
(pat-match '(a (?* ?x) d) '(a b c d))
;; => ((?X B C))

(pat-match '(a (?* ?x) (?* ?y) d) '(a b c d))
;; => ((?Y B C) (?X))

;; => ((?N 3))
