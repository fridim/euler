#lang typed/racket

#| It was proposed by Christian Goldbach that every odd composite number can be
written as the sum of a prime and twice a square.

9 = 7 + 2×1²
15 = 7 + 2×2²
21 = 3 + 2×3²
25 = 7 + 2×3²
27 = 19 + 2×2²
33 = 31 + 2×1²

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime
and twice a square? |#


(require (only-in math next-prime prime?))

(: next-composite (Integer -> Integer))
(define (next-composite n)
  (let ((next (+ 2 n)))
    (if (prime? next)
      (next-composite next)
      next)))

(define (euler46)
  (let: loop : Integer
        ([n : Integer 35] [p : Integer  2])
    (cond ((> p n) n)
          ((integer? (sqrt (/ (- n p) 2)))
           (loop (next-composite n) 2))
          (else (loop n (next-prime p))))))

(time (euler46)) ; 200 ms
