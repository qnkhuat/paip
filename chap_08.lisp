(load "common.lisp")

(defun variable-p (exp)
 "Variables are the symbols M through Z."
 ;; put x,y,z first to find them a little faster
 (member exp '(x y z m n o p q r s t u v w)))

(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

(defun rule-pattern (rule) (first rule))
(defun rule-response (rule) (second rule))

(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
          '(((x+ = y+) (= x y))
            ((- x+) (- x))
            ((+ x+)  (+ x))
            ((x+ + y+) (+ x y))
            ((x+ - y+) (- x y))
            ((x+ * y+) (* x y))
            ((x+ / y+) (/ x y))
            ((x+ ^ y+) (^ x y))))
  "A list of rules, ordered by precedence.")

(defstruct (rule (:type list)) pattern response)

(defstruct (exp (:type list)
                (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun exp-p (x) (consp x))
(defun exp-args (x) (rest x))
(defun binary-exp-p (x)
    (and (exp-p x)
         (= (length (exp-args x)) 2)))
(defun prefix->infix (exp)
  (if (atom exp) exp
      (mapcar #'prefix->infix
              (if (binary-exp-p exp)
                  (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
                  exp))))

(defun infix->prefix (exp)
  "Translate an infix expression into prefix notation"
  (cond ((atom exp) exp)

        ((= 1 (length exp)) (infix->prefix (first exp)))

        ((rule-based-translator exp *infix->prefix-rules*
                                :rule-if #'rule-pattern :rule-then #'rule-response
                                :action #'(lambda (bindings response)
                                            (sublis (mapcar #'(lambda (pair)
                                                                (cons (first pair)
                                                                      (infix->prefix (rest pair))))
                                                            bindings)
                                                    response))))

        ((symbolp (first exp))
         (list (first exp) (infix->prefix (rest exp))))
        (t (error "Illegal exp"))))

(defun ^ (x y) "Exponentiation" (expt x y))

(defparameter *simplification-rules*
  (mapcar #'infix->prefix
          '((x + 0 = x)
            (0 + x = x)
            (x + x = 2 * x)
            (x - 0 = x)
            (0 - x = - x)
            (x - x = 0)
            (- - x = x)
            (x * 1 = x)
            (x * x = x)
            (x * 0 = 0)
            (x * x = x)
            (x * x = x ^ 2)
            (x / 0 = undefined)
            (0 / x = 0)
            (x / 1 = x)
            (x / x = 1)
            (0 ^ 0 = undefined)
            (x ^ 0 = 1)
            (0 ^ x = 0)
            (1 ^ x = 1)
            (x ^ 1 = x)
            (x ^ - 1 = 1 / x)
            (x *(y / x) = y)
            ((y / x)* x = y)
            ((y * x) / x = y)
            ((x * y) / x = y)
            (x + - x = 0)
            ((- x) + x = 0)
            (x + y - x = y))))

(declaim (simplify-exp simplify))

(defun simplify (exp)
  (if (atom exp) exp
      (simplify-exp (mapcar #'simplify exp))))

(defun simplify-exp (exp)
  "Simplify using a rule, or by doing arithmetic."
  (cond ((rule-based-translator exp *simplification-rules*
           :rule-if #'exp-lhs :rule-then #'exp-rhs
           :action #'(lambda (bindings response)
                       (simplify (sublis bindings response)))))
        ((evaluable exp) (eval exp))
        (t exp)))

(defun evaluable (exp)
  "Is this an arithmetic expression that can be evaluated?"
  (and (every #'numberp (exp-args exp))
       (or (member (exp-op exp) '(+ - * /))
           (and (eq (exp-op exp) '^)
                (integerp (second (exp-args exp)))))))

(defun simp (inf) (funcall (comp #'prefix->infix #'simplify #'infix->prefix) inf))

(defun simplifier ()
  (loop
    (print 'simplifier>)
    (print (simp (read)))))

(simp '(100 + 2 * 3))
(simp '(3 * 2 * x))
