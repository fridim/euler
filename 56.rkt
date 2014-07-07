#lang racket/base

#| A googol (10^100) is a massive number: one followed by one-hundred zeros;
100^100 is almost unimaginably large: one followed by two-hundred zeros.
Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, a^b, where a, b < 100, what is
the maximum digital sum?

Answer: c22abfa379f38b5b0411bc11fa9bf92f |#

(require racket/string)

(define (sum-digits n)
  (apply + (filter number?
                   (map string->number
                        (string-split
                          (number->string n)
                          "")))))

(time
  (for*/fold ((m 0)) ((a (in-range 0 100))
                      (b (in-range 0 100))
                      (d (in-value (sum-digits (expt a b))))
                      #:when (> d m))
             d)) ; 1100 ms
