#lang racket

#|
A perfect number is a number for which the sum of its proper divisors is exactly
equal to the number. For example, the sum of the proper divisors of 28 would be
1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123 can
be written as the sum of two abundant numbers. However, this upper limit cannot
be reduced any further by analysis even though it is known that the greatest
number that cannot be expressed as the sum of two abundant numbers is less than
this limit.

Find the sum of all the positive integers which cannot be written as the sum of
two abundant numbers.
|#

;; Just in case...
(custodian-limit-memory (current-custodian) (* 1024 1024 300)) ;; limit to 300MB

(require rackunit
         math/number-theory)

(define limit 28123)
;;(define limit 1000)

(define (is-abundant? n)
  (> (- (apply + (divisors n)) n) n))

(define abundants
  (for/set ([i (in-range limit)]
            #:when (is-abundant? i))
           i))

(define (sum-of-2-abundants? n)
  (for/or ([i (in-set abundants)]
           #:when (< i n))
          (set-member? abundants (- n i))))

(define (euler23)
  (for/sum ([i (in-range limit)]
            #:when (not (sum-of-2-abundants? i))) i))

(time (euler23)) ;; 5.5 s
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((first-abundants
        (set 12 18 20 24 30 36 40 42 48 54 56 60
             66 70 72 78 80 84 88 90 96 100 102)))
  (check-equal?
    (set-intersect first-abundants abundants)
    first-abundants))
