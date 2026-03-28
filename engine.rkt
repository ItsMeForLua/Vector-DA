#lang racket
;; Our ultimate goal here is to evaluate-
;; trigonometric relationships using vector dimensional analysis.
;; We are representing the sides of triangles as-
;; vectors on their own axis so that we can track the "dimensions" of the triangle.

;; For example: side 'a' carries the unit of the a-axis, etc.

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

;; We will now start by defining our base axes
;; A unit is represented as an association list: '((axis-name . power))
;; note that the example given for an association lisp in scheme, is also-
;; a standard cons cell in Lisp
;; However, in order for us to do this, we must use a Church pair.
;; Essentially, it's a function which hugs your two values and waits for you to ask "Can I have those back?"
;; \text{cons} = \lambda x. \lambda y. \lambda f. f\ x\ y
;; Thus, we can see that a pair is not just a static box in memory-
;; rather, it is a "closure". A function that remembers the environment in which it was created.
(define a-axis '((a . 1)))
(define b-axis '((b . 1)))
(define c-axis '((c . 1)))
;; We are assigning an axis as a dimension of 1.
;; such that the quantity is part number and part dimension of 1.
;; and that pair (number . dimension) is what defines our axis.
;; This can lead to: (a . 1) multiplied by (a . 1) will become (a . 2).
;; Or in algebra: x^1 \cdot x^1 = x^2.


;; Example: A quantity of 5 along the c-axis
;; (define my-vector (quantity 5 c-axis))

;; (displayln my-vector)

(define (q-multiply q1 q2)
  (quantity (* (quantity-value q1) (quantity-value q2))
            (append (quantity-units q1) (quantity-units q2))))