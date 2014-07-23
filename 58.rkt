#lang racket/base

#| Starting with 1 and spiralling anticlockwise in the following way, a square
spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right
diagonal, but what is more interesting is that 8 out of the 13 numbers lying
along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, a square spiral
with side length 9 will be formed. If this process is continued, what is the
side length of the square spiral for which the ratio of primes along both
diagonals first falls below 10%? |#

(require (only-in math prime?))

(define (euler58)
  (let loop ((n 2)      ; numbers in diagonal  start with 1,3
             (primes 1) ; numbers of primes in diagonal
             (prev 3))  ; previous number in diagonal
    (if (and (= 1 (modulo n 4)) ; check only squares
             (< (/ primes n) 0.1))
      (sqrt prev)
      (let ((diagn (+ prev (* 2 (+ 1 (quotient (- n 1) 4))))))
        (loop (add1 n)
              (if (prime? diagn) (add1 primes) primes)
              diagn)))))

(time (euler58)) ; 907ms