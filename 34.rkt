#lang racket/base

#| 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their
digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included. |#

(define (number->list n)
  (map (lambda (i) (- (char->integer i) 48)) (string->list (number->string n))))

(define (digit-fact n)
  (list-ref (list 1 1 2 6 24 120 720 5040 40320 362880 3628800) n))

(define (keep? n)
  (= n
     (apply + (map digit-fact (number->list n)))))

(time (for/sum ([i (in-range 10 50000)]
                #:when (keep? i))
               i)) ; 50 ms
