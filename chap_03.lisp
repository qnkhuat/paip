(load "common.lisp")

(defstruct name
  first
  (middle nil)
  last)
;; this will automatically define these functions
;; - make-name : initializer
;; - name-p: recognizer
;; - name-first, name-middle, name-last: accessor
(setf my-name (make-name :first "Ngoc" :last "Khuat"))
;; => #S(NAME :FIRST "Ngoc" :MIDDLE NIL :LAST "Khuat")
(name-first my-name)
;; => "Ngoc"
(name-p my-name)
;; => T

(when T
  (print "Ngoc")
  (print "Quang")
  (print "Khuat")
  nil)


(let ((a 1)
      (b (+ a 1)))) ;; compile error bc 'a' in not bound during b initialization

(let* ((a 1)
       (b (+ a 1)))
  b)

;; Ex 3.1:

(let* ((x 3)
       (y (* x x)))
  y)

((lambda (x)
   ((lambda (y)
      (* y y))
    x))
 3)

;; mapc vs mapcar
;; mapc returns the original list, not the result of applying f with a list like mapcar
(mapc #'(lambda (x) (+ x 1)) '(1 2 3))
;; => (1 2 3)
(mapcar #'(lambda (x) (+ x 1)) '(1 2 3))
;; => (2 3 4)

(defun f (x) (> x 10))

(do ((cnt 1 (+ cnt 1)))
  ((= cnt 10) cnt))


(defun length4 (l)
  (loop for element in l
        count t))

(length4  '(1 2 3))

(defun length6 (l)
  (loop with len = 0
        until (null l)
        for element = (pop l)
        do (incf len)
        finally (return len)))

(length6 '(1 2 3))

(defun length7 (l)
 (count-if #'true l))

(defun true (x) t)

(length7 '(1 2 3))

(princ #\.)

(princ)

(defun print-dot-example ()
  (format t "The dot is: ~c~%" #\.))

(print-dot-example)

;; Ex 3.3:
(defun print-pair
  (items)
  (cond
    ((atom items)
     (princ items))

    ((= (length items) 2)
     (progn
       (princ "(")
       (princ (first items))
       (princ " . ")
       (princ (nth 1 items))
       (princ ")")))
    (t
      (progn
        (princ "(")
        (loop for item in items
              do
              (print-pair item)
              (princ " "))
        (princ ")"))))
  nil)

(print-pair '(1 2 3))


(atom? 1)

(atom 1)


(print (cons 'a 'b))

(princ "a" "b")

(length '( 1 2))

;; predicate

(= 1 1)

;; fail, equality does not work with non-number
(= "a" "a")

;; there are 4 equality predicates
;; eq, eql, equal, equalp
;; eq and eql is basically the same
;; equal is a like a deep equality where it can compares value of a list
;; equalp is equal but it ignores case sensitivity

(eq '( 1 2 ) '( 1 2))
;; => NIL

(equal '( 1 2 ) '( 1 2))
;; => T

(equal "xY" "Xy")
;; => NIL

(equalp "xY" "Xy")
;; => T

;; common lisp has association list which is like a hashmap
(setf state-table '((AL . Alabama)
                    (AK . Alaska)))

;; to get the the key value pair, use assoc
(assoc 'AK state-table)
;; => (AK . ALASKA)

(cdr (assoc 'AK state-table))
;; => ALASKA

;; search by value
(rassoc 'Alabama state-table)
;; => (AL . ALABAMA)

;; but better, they have has table

(setf table (make-hash-table))

(setf (gethash 'AL table) 'Alabama)
;; => ALABAMA
(setf (gethash 'AK table) 'ALASKA)
;; => ALASKA

(maphash (lambda (key value)
           (princ key)
           (princ " - ")
           (princ value)
           (princ " | "))
         table)

(gethash 'AK table)
;; => ALASKA T

(get 'AK table)

;; remove one key/pair
(remhash 'AK table)
;; => T

;; remove everything
(clrhash table)

;; CL has anoter way to represent table: property list or alternating key value pairs

;; property of a symbol is retrieved using the get function
(setf (get 'AL 'state) 'Alabama)
;; => ALABAMA
(get 'AL 'state)
;; => ALABAMA

(setf tree '((a b) ((c)) (d e)))

(tree-equal tree (copy-tree tree))
;; => T

(subst 'new 'old '(old ((very old))))
;; => (NEW ((VERY NEW)))

(sublis '((old . new) (very . pretty)) '(old ((very old))))
;; => (NEW ((PRETTY NEW)))


;; SET
(intersection '(a b c d) '( c d  e))
;; => (D C)
(union '(a b c d) '( c d  e))
;; => (B A C D E)

(with-open-stream (stream (open "test.txt" :direction :output))
  (princ '(hello world) stream))

(with-open-stream (stream (open "test.txt" :direction :input))
  (read stream))
;; => (HELLO WORLD)


;; str format

;; ~& is like \n
(format t "sup~&bro")
;; ~a print like princ would
(format t "hey ~a ya" "ha")
;; ~s print like print1 would, it doesn't escape str
(format t "hey ~s ya" "ha")
;; ~f print number in floating point format
(format t "Yo ~f" 123.12)


;; error handling
(defun average (numbers)
  (if (null numbers)
    ;; like throw in clojure
    (error "Average of the empty list is undefined")
    (/ (reduce #'+ numbers)
       (length numbers))))

(average nil)

(defun average-continue (numbers)
  (if (null numbers)
    (progn (cerror "use 0 as average"
                   "Average of the empty list is undefined")
           0)
    (/ (reduce #'+ numbers)
       (length numbers))))

(average-continue nil)

;; there are also `ecase` where if no case are matched, it'll throw an error
;; `ccase` will throw an contiunable error
;; check-type and assert are also useful

(defun do-that
  (x)
  (assert (numberp x) (x))
  (+ x 1))

(time (do-that 3))

;; lexical scoped is the scope determined by the lexical (source code), whereas dynamic scope is determined by the scope
;; in which a funciton is called in

;; the lexical scope of lisp is quite special.
;; consider the scope of this `bank-account` function
(defun saving-account (balance)
  #'(lambda (new-amount)
      (setf balance (+ balance new-amount))))

(setf my-account (saving-account 300))
(funcall my-account 100)
;; => 400
(funcall my-account 100)
;; => 500
;; notice here the lifetime of `balance` exists even after the closure my-account is created

;; SPECIAL VARIABLE

(defvar *counter* 0) ;; special variable can be defined by defvar or defparameter

(defun report-count ()
  (format nil "Counter = ~a" *counter*))

(report-count)
;; => "Counter = 0"

(let ((*counter* 3))
  (report-count))
;; => "Counter = 3"
;; a special varaible can be shadowed by local binding
;; basically it's like dynamic variable in clojure

(+ 1 2 3)

;; multiple values

(defun hey ()
  (values 1 2))

(multiple-value-bind (int rem) (hey)
  (format t "1st ~a~%" int)
  (format t "2nd ~a~%" rem))

;; Optional and default parameters

(defun do-stuff (&optional (op '+) (fst 1) (snd 2))
  (funcall op fst snd))
(do-stuff)
;; => 3
(do-stuff '-)
;; => -1

(defun do-stuff (&key (op '+) (fst 1) (snd 2))
  (funcall op fst snd))

(do-stuff :fst 2)
;; => 4
