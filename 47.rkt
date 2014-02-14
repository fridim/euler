#lang racket/base

#| The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers? |#

(require (only-in math prime-divisors)
         rackunit)

(define (match? n nb)
  (= nb (length (prime-divisors n))))

(check-true (match? 14 2))
(check-true (match? 15 2))
(check-true (match? 644 3))
(check-true (match? 645 3))
(check-true (match? 646 3))

(define (consecutives n primes-factors)
  (let loop ([i 0])
    (if (match? (+ n i) primes-factors)
      (loop (add1 i))
      i)))

(check-equal? (consecutives 13 2) 0)
(check-equal? (consecutives 14 2) 2)
(check-equal? (consecutives 644 3) 3)

(define (euler47 limit)
  (let loop ([cursor 1])
    (let ((i (consecutives cursor limit)))
      (if (>= i limit)
        cursor
        (loop (+ cursor (add1 i)))))))

(check-equal? (euler47 3) 644)

(time (euler47 4)) ; 2 secondes
