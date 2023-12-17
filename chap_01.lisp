(+ 1 2)
;; => 3

(append '(Khuat Quang) '(Ngoc)) ; (KHUAT QUANG NGOC)

'Ngoc ;; => NGOC

(= "Ngoc" "NGOC")

;; Symbols are not case sensitive, so 'Ngoc, 'NGOC are the same symbol.

(setf name '(Khuat Quang Ngoc))
;; => (KHUAT QUANG NGOC)

name
;; => (KHUAT QUANG NGOC)

(setf name '(Le Thi My Linh))
;; => (LE THI MY LINH)

name
;; => (LE THI MY LINH)

(defun last-name (name)
  "Return the last name of a name."
  (first (last name)))
;; => LAST-NAME

(last-name '(Khuat Quang Ngoc))
;; => NGOC

(setf names (list '(Khuat Quang Ngoc) '(Le Thi My Linh)))
;; => ((KHUAT QUANG NGOC) (LE THI MY LINH))

(mapcar #'last-name names)
;; => (NGOC LINH)

;; car = Content of address register
;; cdr = Content of decrement register
;; this refer to the instructions that were used to implement the 1st Lisp on IBM 704

(mapcar #'+ '(1 2 3 4))
;; => (1 2 3 4)

(mapcar #'+ '(1 2 3 4) '(10 20 30 40))
;; => (11 22 33 44)


(defparameter *titles*
  '(Mr Mrs Miss))

(defun first-name
  (name)
  (let ((first-name (first name)))
    (if (member first-name *titles*)
      (first-name (rest name))
      first-name)))

(first-name '(Mr Ngoc))
;; => NGOC

(trace first-name)

(first-name '(Mr Miss ABC))
;;  0: (FIRST-NAME (MR MISS ABC))
;;   1: (FIRST-NAME (MISS ABC))
;;     2: (FIRST-NAME (ABC))
;;     2: FIRST-NAME returned ABC
;;   1: FIRST-NAME returned ABC
;;  0: FIRST-NAME returned ABC

;; Ex: 1.2
(defun power (base exponent)
  (if (= exponent 0)
    1
    (* (power base (- exponent 1)) base)))

(power 3 3)

(atom (list 1 2 3))
(atom nil)
;; => 27

;; Ex: 1.4
(defun count-anywhere
  (target items)
  (reduce #'(lambda (cnt x)
              (if (atom x)
                (if (eql x target)
                  (+ cnt 1)
                  cnt)
                (+ cnt (count-anywhere target x))))
          items
          :initial-value 0))

(count-anywhere 'a '(a ((a) b) a))
;; => 3


;; Ex: 1.5

(defun dot-product
  (a b)
  (apply #'+ (mapcar #'* a b)))

(dot-product '(10 20) '(3 4))
;; => 110
