;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;; a² + b² = c²
;; 
;; For example, 3² + 4² = 9 + 16 = 25 = 5².
;; 
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.

#lang racket

;; a + b + c = 1000   and a < b < c
;; => b = 1000 - a - c
;; => b in range [a ; 1000 - a - (min c)]
;; a < b < c => b in range [a ; 1000 - a - a]
(define (euler9)
  (for*/first ([a (in-list (range 1 1000))]
               [b (in-list (range a (- 1000 a a)))]
               [c (in-value (- 1000 a b))] 
               #:when (and (= (+ (* a a) (* b b)) (* c c))
                           (< a b c)))
              (* a b c)))
(euler9)
