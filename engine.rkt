#lang racket
;; Our ultimate goal here is to evaluate-
;; trigonometric relationships using vector dimensional analysis.
;; We are representing the sides of triangles as-
;; vectors on their own axis so that we can track the "dimensions" of the triangle.

;; For example: side 'a' carries the unit of the a-axis, etc.

;; My philosophy in math is a preference for "unit-first thinking", rather than "formula-first thinking"-
;; Formulas are compressed mnumonics that hide their own assumptions. DA simply makes-
;; those assumptions explicit and structural. Because of this, sanity checks aren't an after-the fact numerical calculation-
;; they're structural and natural to the process itself.
;; There are fundamental epistemological differences. With DA, you get reasoning from invariants.

;; ------------------------------------------------------------
;; In this two part structure, our "value" is the number, and-
;; the "units" are a list of dimensions.
;; Note that structs will generate helper functions under the hood, so performance here-
;; is not going to be relevent.

;; For standard math, we throw around naked numbers, but this can be dangerous
;; simply because the numbers themselves have no context explicitly assigned to them.
;; Instead of naked numbers, we will be passing around blocks of "quantity".
;; a blob defined as units, will be the dimensional metadata associated with our value(our number).
;; #:transparent is needed for the struct to (1) not be blackboxed, and-
;; (2), to allow Racket's 'equal?' function to compare the actual values inside the struct.

(struct quantity (value units) #:transparent)

;; -------------------------------------------------------------
;; We will now start by defining our base axes
;; A unit is represented as an association list: '((axis-name . power))
;; note that the example given for an association lisp in scheme, is also-
;; a standard cons cell in Lisp
;; However, in order for us to do this, we must use a Church pair.
;; Essentially, it's a function which hugs your two values and waits for you to ask "Can I have those back?"
;; \text{cons} = \lambda x. \lambda y. \lambda f. f\ x\ y
;; Thus, we can see that a pair is not just a static box in memory-
;; rather, it is similar to (but not literally) a "closure". A function that remembers the environment in which it was created.
(define a-axis '((a . 1)))
(define b-axis '((b . 1)))
(define c-axis '((c . 1)))
;; We are assigning an axis as a dimension of 1.
;; such that the quantity is part number and part dimension of 1.
;; and that pair (number . dimension) is what defines our axis.
;; This can lead to: (a . 1) multiplied by (a . 1) which will become (a . 2).
;; Or in algebra: x^1 \cdot x^1 = x^2.
;; Example: A quantity of 5 along the c-axis
;; (define my-vector (quantity 5 c-axis))
;; (displayln my-vector)

;; -------------------------------------------------------------

(define (q-multiply q1 q2)
  (quantity (* (quantity-value q1) (quantity-value q2))
            (simplify-units (append (quantity-units q1) (quantity-units q2)))))
;; simplify units is how we're storing the result of q-multiply, so that we can-
;; continue to clean up the results in a for/fold block
(define (simplify-units messy-units)
;; We will run the fold and store the resulting hash table in "merged-hash"
(define merged-hash
(for/fold 
([backpack (hash)]) ;; Here we first start with an empty hash.
([item messy-units]) ;; Here we're saying to walk through the items of our list
;; note that "item" is a pair such as (a . 1)
;; our car item is the axis name (a) and our cdr item in the power of 1.

;; -------------------------------------------------------------
;; hash-update takes the backpack, looks for the axis name, and adds the power to-
;; whatever is already there. 
;; The 0 at the end tells Racket-
;; "If this axis isn't in the backpack yet, assume its power is 0."
(hash-update backpack (car item)
;; It's important to note here that hash-update does NOT mutate the hashtable. 
;; It simply reads our hashtable, applies the math we defined, and gives us a NEW hash table.
;; here, our hashtable is actually named "backpack"
;; (car item) extracts just the car item of our pairs. Or in otherwords "find the a-axis"

(lambda (current-power) (+ current-power (cdr item)))
0))) ;; "other-wise zero"

(hash->list merged-hash)) ;; our final sum

(define (q-add q1 q2)
  (if (equal? (quantity-units q1) (quantity-units q2))
    (quantity (+ (quantity-value q1) (quantity-value q2))
              (quantity-units q1)) ;; If we didn't include this, our engine would just spit out a naked 12.
    (error "Dimensional Mismatch: You cannot add these two quantities!")))

(define (q-subtract q1 q2)
  (if (equal? (quantity-units q1) (quantity-units q2))
    (quantity (- (quantity-value q1) (quantity-value q2))
              (quantity-units q1))
    (error "Dimensional Mismatch: You cannot subtract these two quantities!")))