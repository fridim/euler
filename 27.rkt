#lang racket/base

#|
Euler discovered the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values
n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible
by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes
for the consecutive values n = 0 to 79. The product of the coefficients, −79 and
1601, is −126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that
produces the maximum number of primes for consecutive values of n, starting with
n = 0.
|#

(require rackunit
         math
         (only-in racket/list range))

(define (max-consecutive-prime a b)
  (define (quad n)
    (+ (* n n) (* a n) b))
  (let helper ([n 1])
    (let ((qn (quad n)))
      (if (< qn 2)
        n
        (if (prime? qn)
          (helper (+ n 1))
          n)))))

(check-equal? (max-consecutive-prime 1 41) 40)
(check-equal? (max-consecutive-prime -79 1601) 80)

(define (euler27 limit)
  ; (n = 0) b has to be prime and positive
  ; (n = 1) 1 + a + b must be prime => a has to be odd
  (let ((blist (filter prime? (range 2 limit)))
        (alist (filter odd? (range (- limit) limit))))
    (for*/fold ([ab 0]
                [max-cons 0])
               ([b (in-list blist)]
                [a (in-list alist)])
               (let ((c-cons (max-consecutive-prime a b)))
                 (if (> c-cons max-cons)
                   (values (* a b) c-cons)
                   (values ab max-cons))))))

(time (euler27 999)) ; cpu time: 300ms
