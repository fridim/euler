#lang racket/base

#| It is possible to show that the square root of two can be expressed as an
infinite continued fraction.

√ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

By expanding this for the first four iterations, we get:

1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

The next three expansions are 99/70, 239/169, and 577/408, but the eighth
expansion, 1393/985, is the first example where the number of digits in the
numerator exceeds the number of digits in the denominator.

In the first one-thousand expansions, how many fractions contain a numerator
with more digits than denominator? |#

(define (sqrt2 rounds)
  (let loop ((i rounds))
    (if (= i rounds)
      (+ 1 (/ 1 (loop (- i 1))))
      (if (= 0 i)
        2
        (+ 2 (/ 1 (loop (- i 1))))))))

(require rackunit
         (only-in math exact-ceiling))

(check-equal? (sqrt2 5) 99/70)
(check-equal? (sqrt2 6) 239/169)
(check-equal? (sqrt2 7) 577/408)

(define (count-digits n)
  (if (<= n 1)
    1
    (abs (exact-ceiling (/ (log n) (log 10))))))

(define (euler57)
  (length
    (filter (λ(n) (> (count-digits (numerator n)) (count-digits (denominator n))))
            (for/list ((i (in-range 1 1001)))
                      (sqrt2 i)))))

(time (euler57)) ; 304ms
