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
