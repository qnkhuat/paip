(load "common.lisp")
;; Sentence => Noun-Phrase + Verb-Phrase
;; Noun-Phrase => Article + Noun
;; Verb-Phrase => Verb + Noun-Phrase
;; Article => "the" | "a"
;; Noun => man | ball | women | table
;; Verb => hit | took | saw | liked

;; ------------------------- A simple solution

(defun sentence
  ()
  (append (noun-phrase) (verb-phrase)))

(defun noun-phrase
  ()
  (append (article) (noun)))

(defun verb-phrase
  ()
  (append (verb) (noun-phrase)))

(defun article
  ()
  (random-elt '(the a)))

(defun article
 ()
 (random-elt '(the a)))

(defun noun
 ()
 (random-elt '(man ball women table)))

(defun verb
  ()
  (random-elt '(hit took saw liked)))

(defun random-elt
  (choices)
  (list (elt choices (random (length choices)))))

(sentence)
;; => (A WOMEN HIT A MAN)

;; ------------------------- Rule based solution

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball women table)
    (Verb -> hit took saw liked)))

(defvar *grammar* *simple-grammar*)

;; find the 1st rule that matches the given symbol
(assoc 'sentence *grammar*)
;; => (SENTENCE -> (NOUN-PHRASE VERB-PHRASE))

(defun rule-lhs
  (rule)
  (first rule))

(defun rule-rhs
  (rule)
  (rest (rest rule)))

(defun rewrites (category)
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  (cond ((listp phrase)
         (mappend #'generate phrase))

        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))

        (t (list phrase))))

(generate 'sentence)
;; => (THE TABLE HIT A TABLE)


(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

(trace generate)
(untrace generate)

(generate 'sentence)
;; => (A MAN WITH A BIG BIG ADIABATIC WOMAN HIT PAT ON THOSE)


(defun generate-tree (phrase)
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))

        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))

        (t (list phrase))))

(setf *grammar* *simple-grammar*)
(trace generate-tree)
(generate-tree 'sentence)
(untrace generate-tree)

(defun generate-all (phrase)
  (cond ((null phrase) (list nil))

        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))

        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))

        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x
  E.g: (combine-all '((a) (b)) '((1) (2)))

  -> ((A 1) (B 1) (A 2) (B 2))"
  (mappend #'(lambda (x) (mapcar #'(lambda (y) (append x y)) ylist)) xlist))

(length (generate-all 'sentence))
;; => 256

(trace generate-all)
(generate-all 'NOUN-PHRASE)
(untrace generate-all)


;; Ex 2.3:
(defun cross-product (fn)
  (lambda (xlist ylist) (mappend #'(lambda (x) (mapcar #'(lambda (y) (funcall fn x y)) ylist)) xlist)))

(funcall (cross-product #'append) '((a) (b)) '((1) (2)))
;; => ((A 1) (A 2) (B 1) (B 2))

(funcall (cross-product #'+) '(1 2 3) '(10 20 30))
;; => (11 21 31 12 22 32 13 23 33)
