;; Interactive programs
(load "common.lisp")

(defun interactive-interpreter (promp transformer)
  (loop
    (print (prompt))
    (print (funcall transformer (read)))))

(defun lisp ()
  (interactive-interpreter '> #'eval))

(defun eliza ()
  (interactive-interpreter 'eliza> #'(lambda (x) (flatten (use-eliza-rules x)))))

(defun interactive-interpreter (prompt transformer)
  "Read an expression, transform it, and print the result."
  (loop
    (handler-case
      (progn
        (if (stringp prompt)
          (print prompt)
          (funcall prompt))
        (print (funcall transformer (read))))
      ;; In case of error, do this:
      (error (condition)
             (format t "~&;; Error ~a ignored, back to top level."
                     condition)))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  "Return a function that prints prompts like [l], [2], etc."
  #'(lambda () (format t ctl-string (incf num))))

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


(pat-match '(a (?* ?x) d) '(a b c d))
;; => ((?X B C))

(pat-match '(a (?* ?x) (?* ?y) d) '(a b c d))
;; => ((?Y B C) (?X))

;(pat-match  '(?x ?op ?y is ?z (?if (eql (?op ?x ?y) ?z))) '(3 + 4 is 7))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
    (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
      ((atom pat) pat)
      (t (cons (expand-pat-match-abbrev (first pat))
          (expand-pat-match-abbrev (rest pat))))))


;; 6.3 rule based translator tool
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

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (rule-based-translator input *eliza-rules*
    :action #'(lambda (bindings responses)
          (sublis (switch-viewpoint bindings)
                (random-elt responses)))))

;; 6.4 set of searching tools
;; A search problem can be characterized by four features:
;; - The start state
;; - The goal state
;; - The successors, or states that can be reached from any other state
;; - The strategy that determines the order in which we search
(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p. Start with states, and search according to successors and combiners"
  (dbg :search "~&; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
             (funcall combiner (funcall successors (first states))
                      (rest states))
             goal-p successors combiner))))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))

(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))

(defun is (value) #'(lambda (x) (eql x value)))
(debug* :search)

;(depth-first-search 1 (is 12) #'binary-tree)

(defun breadth-first-search
  (start goal-p successors)
  (tree-search (list start) goal-p successors #'prepend) )

(breadth-first-search 1 (is 12) #'binary-tree)

(defun finite-binary-tree (n)
  "Return a successor function that generates a binary tree
  with n nodes."
  #'(lambda (x)
      (remove-if #'(lambda (child) (> child n))
                 (binary-tree x))))

(depth-first-search 1 (is 12) (finite-binary-tree 15))

;; best first search
(defun diff (num)
  #'(lambda (x) (abs (- x num))))

(defun sorter (cost-fn)
  #'(lambda (new old) (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors cost-fn)
  (tree-search (list start) goal-p successors (sorter cost-fn)))

;; because we know about the state space, we can inject our knowledge into the general search algorithm
(best-first-search 1 (is 12) #'binary-tree (diff 12))


;; like best-first-search but it does not let the search space to grow indefnitely
;; instead it'll only keep `beam-width` items after sort
;; it's like we only look down several paths at once and chooses the best one to look at next
;; One draw back is that we lose the ability to backtrack
(defun beam-search (start goal-p successors cost-fn beam-width)
  (tree-search (list start) goal-p successors
               #'(lambda (old new)
                   (let ((sorted (funcall (sorter cost-fn) old new)))
                     (if (> beam-width (length sorted))
                       sorted
                       (subseq sorted 0 beam-width))))))

(defun price-is-right (price)
  "Return a function that measures the difference from price,
  but gives a big penalty for going over price."
  #'(lambda (x) (if (> x price)
              most-positive-fixnum
              (- price x))))

(beam-search 1 (is 12) #'binary-tree (price-is-right 12) 2)

(defstruct (city (:type list)) name long lat)

(defparameter *cities*
  '((Atlanta        84.23 33.45)
    (Los-Angeles       118.15 34.03)
    (Boston           71.05 42.21)      (Memphis           90.03 35.09)
    (Chicago          87.37 41.50)      (New-York          73.58 40.47)
    (Denver           105.00 39.45)     (Oklahoma-City     97.28 35.26)
    (Eugene           123.05 44.03)     (Pittsburgh        79.57 40.27)
    (Flagstaff        111.41 35.13)     (Quebec            71.11 46.49)
    (Grand-Jct        108.37 39.05)     (Reno              119.49 39.30)
    (Houston          105.00 34.00)     (San-Francisco     122.26 37.47)
    (Indianapolis     86.10 39.46)      (Tampa             82.27 27.57)
    (Jacksonville     81.40 30.22)      (Victoria          123.21 48.25)
    (Kansas-City      94.35 39.06)      (Wilmington        77.57 34.14)))

(defun neighbors (city)
  (find-all-if #'(lambda (c)
                   (and (not (eq c city))
                        (< (air-distance c city) 1000.0)))
               *cities*))

(defun city (name)
  (assoc name *cities*))

(defconstant earth-diameter 12765.0
  "Diameter of planet earth in kilometers.")

(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;; d is the straight-line chord between the two cities,
    ;; The length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere.
  The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
      (list (* (cos psi) (cos phi))
            (* (cos psi) (sin phi))
            (sin psi))))

(defun distance (point1 point2)
  "The Euclidean distance between two points.
  The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                point1 point2))))

(defun deg->radians (deg)
  "Convert degrees and minutes to radians."
  (* (+ (truncate deg) (* (rem  deg 1) 100/60)) pi 1/180))

(defun trip (start dest)
  (beam-search start (is dest) #'neighbors #'(lambda (c) (air-distance c dest)) 1))

(untrace neighbors)
(untrace air-distance)

(trip (city 'san-francisco) (city 'boston))
; Search: ((SAN-FRANCISCO 122.26 37.47))
; Search: ((RENO 119.49 39.3))
; Search: ((GRAND-JCT 108.37 39.05))
; Search: ((DENVER 105.0 39.45))
; Search: ((KANSAS-CITY 94.35 39.06))
; Search: ((INDIANAPOLIS 86.1 39.46))
; Search: ((PITTSBURGH 79.57 40.27))
; Search: ((BOSTON 71.05 42.21))
;; => (BOSTON 71.05 42.21)
;; this works fine but it does a bunch of small trips, the total total trip might be much more than we should
;; so we'll work on a more optiomal solution

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~a>"
        (path-state path) (path-total-cost path)))

(defun map-path (fn path)
  "Call fn on each state in the path, collecting results."
  (if (null path)
      nil
      (cons (funcall fn (path-state path))
          (map-path fn (path-previous path)))))

(defun show-city-path (path &optional (stream t))
  "Show the length of a path, and the cities along it."
  (format stream "#<Path ~a km: ~{~:(~a~)~^- ~}>"
        (path-total-cost path)
        (reverse (map-path #'city-name path)))
  (values))

(defstruct (path (:print-function print-path))
  state
  (previous nil)
  (cost-so-far 0)
  (total-cost 0))

(defconstant earth-diameter 12765.0
  "Diameter of planet earth in kilometers.")

(defun distance (point1 point2)
  "The Euclidean distance between two points.
  The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))

(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;; d is the straight-line chord between the two cities,
    ;; The length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere.
  The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
      (list (* (cos psi) (cos phi))
            (* (cos psi) (sin phi))
            (sin psi))))

(defun deg->radians (deg)
  "Convert degrees and minutes to radians."
  (* (+ (truncate deg) (* (rem  deg 1) 100/60)) pi 1/180))

(defun is (target &key (key #'identity) (test #'eql))
  #'(lambda (x) (funcall test target (funcall key x))))

(defun path-saver (successors cost-fn cost-left-fn)
  #'(lambda (old-path)
      (let ((old-state (path-state old-path)))
        (mapcar
          #'(lambda (new-state)
              (let ((old-cost (+ (path-cost-so-far old-path)
                                 (funcall cost-fn old-state new-state))))
                (make-path
                  :state new-state
                  :previous old-path
                  :cost-so-far old-cost
                  :total-cost (+ old-cost (funcall cost-left-fn new-state)))))
          (funcall successors old-state)))))

(defun trip (start dest &optional (beam-width 1))
  "Search for the best path from the start to dest"
  (beam-search (make-path :state start)
               (is dest :key #'path-state)
               (path-saver #'neighbors #'air-distance
                           #'(lambda (c) (air-distance c dest)))
               #'path-total-cost
               beam-width))

(trip (city 'san-francisco) (city 'boston))
;; => #<Path to (BOSTON 71.05 42.21) cost 4514.8366418661035d0>

(defun iter-wide-search (start goal-p successors cost-fn
                &key (width 1) (max 100))
  "Search, increasing beam width from width to max.
  Return the first solution found at any width."
  (dbg :search "; Width: ~d" width)
  (unless (> width max)
    (or (beam-search start goal-p successors cost-fn width)
      (iter-wide-search start goal-p successors cost-fn
                        :width (+ width 1) :max max))))

;; The city problem is not a tree search problem but a graph search
;; but using tree-search works because you can represent a graph problem as a treen problem
;; one downside is that in a graph you have limited number of path you can follow, but if you
;; turn it into a tree problem you'll have much more path and potentialy infinite

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen before"
  (remove-if #'(lambda (state)
                 (or (member state states :test state=)
                     (member state old-states :test state=)))
             (funcall successors (first states))))

(defun graph-search (states goal-p successors combiner &optional (state= #'eql) old-states)
 "Find a state that satisfies goal-p. Start with states,and search according to successors and combiner.
  Don't try the same state twice."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (graph-search
            (funcall
              combiner
              (new-states states successors state= old-states)
              (rest states))
            goal-p successors combiner state=
            (adjoin (first states) old-states
                      :test state=)))))

(debug* :search)

(defun next2 (x) (list (+ x 1) (+ x 2)))

(tree-search '(1) (is 6) #'next2 #'prepend)
;; Search: (1)
;; Search: (3 3 4)
;; Search: (2 3)
;; Search: (3 4 4 5)
;; Search: (4 4 5 4 5)
;; Search: (4 5 4 5 5 6)
;; Search: (5 4 5 5 6 5 6)
;; Search: (4 5 5 6 5 6 6 7)
;; Search: (5 5 6 5 6 6 7 5 6)
;; Search: (5 6 5 6 6 7 5 6 6 7)
;; Search: (6 5 6 6 7 5 6 6 7 6 7)
;; => 6

(graph-search '(1) (is 6) #'next2 #'prepend)
;; Search: (1)
;; Search: (2 3)
;; Search: (3 4)
;; Search: (4 5)
;; Search: (5 6)
;; Search: (6 7)
;; => 6

(defun find-path (state paths state=)
  "Find the path with this state among a list of paths."
  (find state paths :key #'path-state :test state=))

(defun better-path (pathl path2)
  "Is path1 cheaper than path2?"
  (< (path-total-cost path1) (path-total-cost path2)))

(defun insert-path (path paths)
  "Put path into the right position, sorted by total cost."
  ;; MERGE is a built-in function
  (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun path-states (path)
  "Collect the states along this path."
  (if (null path)
      nil
      (cons (path-state path)
            (path-states (path-previous path)))))

(defun a*-search (paths goal-p successors cost-fn cost-left-fn
                        &optional (state= #'eql) old-paths)
  "Find a path whose state satisfies goal-p. Start with paths,
  and expand successors, exploring least cost first.
  When there are multiple duplicate states, keep the one with lower cost and discard the other."
  (dbg :search ";; Search: ~a" paths)
  (cond
    ((null paths) fail)
    ((funcall goal-p (path-state (first paths)))
     (values (first paths) paths))
        (t (let* ((path (pop paths))
              (state (path-state path)))
         ;; Update PATHS and OLD-PATHS to reflect
         ;; the new successors of STATE
         (setf old-paths (insert-path path old-paths))
         ;; For each new successor state, there are three possibilities.
         ;; The new state may be in the list of current paths, in the list of old paths, or in neither.
         ;; Within the first two cases, there are two subcases.
         ;; If the new path is more expensive than the old one, then ignore the new path - it can not lead to a better solution.
         ;; If the new path is cheaper than a corresponding path in the list of current paths, then replace it with the new path.
         ;; If it is cheaper than a corresponding path in the list of the old paths, then remove that old path, and put the new
         ;; path in the list of current paths.
         (dolist (state2 (funcall successors state))
           (let* ((cost  (+ (path-cost-so-far path)
                           (funcall cost-fn state state2)))
                  (cost2 (funcall cost-left-fn state2))
                  (path2 (make-path
                           :state state2 :previous path
                           :cost-so-far cost
                           :total-cost (+ cost cost2)))
                  (old   nil))
             ;; Place the new path, path2, in the right list:
             (cond
               ;; new path in the old path, replace if it's a better path
               ((setf old (find-path state2 paths state=))
                (when (better-path path2 old)
                  (setf paths (insert-path path2 (delete old paths)))))
               ((setf old (find-path state2 old-paths state=))
                (when (better-path path2 old)
                  (setf paths (insert-path path2 paths))
                  (setf old-paths (delete old old-paths))))
               (t (setf paths (insert-path path2 paths)))))))
       ;; Finally, call A* again with the updated path lists:
       (a*-search paths goal-p successors cost-fn cost-left-fn state= old-paths))))

(path-states
  (a*-search (list (make-path :state 1)) (is 6)
             #'next2 #'(lambda (x y) 1) (diff 6)))
;; => (6 5 3 1)
;; NOTE: a* search does not do anything special, at each step it considers what is the lowest cost
;; the formula for the cost to reach `n`node is f(n) = g(n) + h(n)
;; - g(n) is the cost so far
;; - h(n) is a heuristic cost funciton where it estimates the cost from n -> target.

;; defining a good h(n) is the most important thing, if a h(n) is admissble heuristic meaning
;; it estimates nevery overestimate then a*search guaranteed to return an optional solution
;; https://brilliant.org/wiki/a-star-search/


;; Ex: 6.11



(trip (city 'san-francisco) (city 'boston))
:; => #<Path to (BOSTON 71.05 42.21) cost 4514.8366418661035d0>


(defun search-n (n start goal-p successors cost-fn beam-width)
  (let ((solutions '()))
    (beam-search start #'(lambda (x)
                           (cond ((not (funcall goal-p x)) nil)
                                 ((= n 0) x)
                                 (t (decf n)
                                    (push x solutions))))
                 successors
                 cost-fn
                 beam-width)
    solutions))

(defun trip-2 (start dest)
  (search-n 2 start (is dest) #'neighbors #'(lambda (c) (air-distance c dest)) 1))

(trip-2 (city 'san-francisco) (city 'boston))
