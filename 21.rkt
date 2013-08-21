#lang racket/base

#| Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n).  If d(a) = b and d(b) = a, where a ≠ b, then a and
b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000. |#

(define (proper-divisors n)
  (let helper ([i 1])
    (cond [(= i n) '()]
          [(= 0 (modulo n i))
           (cons i (helper (+ i 1)))]
          [else (helper (+ i 1))])))

(define (d n)
  (apply + (proper-divisors n)))

(define (amicable? a d-a)
  (= a (d d-a)))

(define (euler21 limit)
  (let sum-amicables ([i 1])
    (if (= i limit) 0
      (let ([d-i (d i)])
        (if (or (>= i d-i) (not (amicable? i d-i)))
          (sum-amicables (+ 1 i))
            (+ i (if (< d-i limit) d-i 0) (sum-amicables (+ 1 i))))))))

(time (euler21 10000))
