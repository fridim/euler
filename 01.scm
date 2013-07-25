#lang racket/base

;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we
;; get 3, 5, 6 and 9. The sum of these multiples is 23.
;; Find the sum of all the multiples of 3 or 5 below 1000.

(define LIMIT 1000)

(define (get-multiples a n)
  (cond
    [(< (* a n) LIMIT )
     (+ (* a n) (get-multiples a (+ n 1)))]
    [else 0]
    ))

(display (- (+ (get-multiples 3 1) (get-multiples 5 1)) (get-multiples 15 1)))
