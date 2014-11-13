;; When we replace variables of a logic statement with truth values,
;; we replace all logic junctors with themselves. This makes
;; replacement easier, since we can just use a subst-* function.
(defparameter logic-symbol-replacement '((and . and) (or . or) (implies . implies) (not . not)))

;; For prettier output, we define f (for false) to be equivalent to
;; nil. This way the output A = F, B = T aligns nicely (A = NIL, B =
;; T breaks our columns).
(defvar f nil)

;; Graham's flatten: Takes a tree and flattens it into a list. E.g.:
;; (flatten '(a (b (c d) e))) evaluates to (A B C D E)
(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

;; Pretty printer for our solutions.
(defun print-solution (variables)
  ; Our solutions come in as ((A . F) (B . T)). First we make a list
  ; out of it: ((A F) (B T)). Then format iterates over the list
  ; (outermost ~{ ~}) and prints each solution on its own line by
  ; iterating over the inner lists (A F), separating the two elements
  ; with "=". A comma is conditionally printed only when the current
  ; element isn't the last one in the list.
  (format t "solution found: ~{~{~a~^ = ~}~^, ~}~%"
	  (mapcar #'(lambda (x) `(,(car x) ,(cdr x))) variables)))

;; Logical implication (since Lisp doesn't come wih it by default)
(defun implies (a b)
  (or (not a) b))

;; Takes a list of combination of truth values (t f ...) and
;; associates it with variables (a b ...) into a list of conses:
;; ((A . T) (B . F) ...)
(defun associate-variables-with-combinations (combinations variables)
  (loop for combination in combinations collect
       (loop for var in variables
	  for number in combination
	  collect (cons var number))))

;; Test if the elements of objs are eq to obj1. test can be any logic
;; function (e.g. "and" or "or")
(defmacro eqs (test obj1 &rest objs)
  `(,test 
    ,@(loop for o in objs collect
	   (list 'eq obj1 o))))

;; Gets all symbols from a statement which aren't logical junctors
;; (and, or, not, implies), so these can later be substituted with
;; actual values for evaluation.
(defun get-variables-from-statement (statement)
  (remove-duplicates
   (loop for logic-symbol in (flatten statement)
      when (not (eqs or logic-symbol 'and 'or 'not 'implies))
      collect logic-symbol)))

;; Transform a number into a list of bits which represent the numbers
;; value in base 2. E.g (number-to-bitlist 42) evaluates to (1 0 1 0 1 0)
(defun number-to-bitlist (number)
  (reverse
   (loop repeat (integer-length number)
      for i = number then (ash i -1)
      collect (logand i 1))))

;; Creates a list of lists of the numbers 1 and 0. The inner lists
;; contain a binary representation of the numbers from 0 upto number:
;; E.g. for number = 2: ((0 0) (0 1) (1 0) (1 1))
;; The lists are filled, so for 4 values we have 4 bits and the value
;; 2 isn't represented as (1 0) but as (0 0 1 0).
;; Number must be some 2^n - 1
(defun filled-bitlists-upto-number (number)
  ; Make sure number is 2^n - 1
  (assert (= 0 (boole boole-and (1+ number) number)))
  (mapcar
   #'(lambda (x)
       (append
	(make-list (- (ceiling (log number 2)) (length x))
		   :initial-element 0)
	x))
   (loop for i from 0 upto number
      collect (number-to-bitlist i))))

;; Walk tree of statements and replace all smybols according to
;; "symbols" list. If we pass '((a . 1) (b . 0) (c . 1) (and . and)
;; (or . or) (not . not))), a and c will  be replaced with 1, b with
;; 0, and all junctors (and or not) are replaced with themselves.
(defun subst-symbols-by-list (tree symbols)
  (loop for node in tree collect
       (cond ((listp node) (subst-symbols-by-list node symbols))
	     (t (cdr (assoc node symbols))))))

;; takes a number and returns a list of list of ts and fs, e.g.:
;; ((t t t) (t t f) (t f t) ...). This list represents all possible
;; combinations of truth values.
(defun filled-truthlist-upto-number (number)
  (subst-symbols-by-list 
   (filled-bitlists-upto-number number)
   '((1 . t) (0 . f))))

;; Main function: Enter any logic statement with variables and any of
;; the junctors and, or, not, implies as a valid lisp statement. For
;; example: (find-solution '(or (and a (not b)) (and b c)))
(defun find-solution (logic-statement)
  (let* ((variables (get-variables-from-statement logic-statement))
	 (combinations (filled-truthlist-upto-number (1- (expt 2 (length variables)))))
	 (replacement-lists (associate-variables-with-combinations combinations variables)))
    (loop for replacement in replacement-lists do
	 (if (eql t (eval
		     (subst-symbols-by-list logic-statement
					    (append replacement logic-symbol-replacement))))
	     (print-solution replacement)))))
    