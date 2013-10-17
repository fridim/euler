#lang racket/base

#|
Starting with the number 1 and moving to the right in a clockwise direction a 5
by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed
in the same way?
|#

(require rackunit)

(define (euler28 limit)
  (+ 1 (let helper ([n 0] [sum 0] [prev 1])
         (let ((diagn (+ prev (* 2 (+ 1 (quotient n 4))))))
           (if (> diagn (* limit limit))
             sum
             (helper (+ n 1) (+ sum diagn) diagn))))))

(check-equal? (euler28 5) 101)

(time (euler28 1001)) ; 0ms
