#lang racket/base

#| If p is the perimeter of a right angle triangle with integral length sides,
{a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised? |#

;; Use the following :
;; p = a + b + c
;; a² + b² = c²
(define (sol p)
  (for*/list ([a (in-range 1 p)]
              [b (in-range (- p a) a -1)]
              [c (in-value (- p a b))]
              #:when (= (+ (* a a) (* b b))
                        (* c c)))
             (list a b c)))

(require rackunit)

(check-equal? (sol 120) (list (list 20 48 52)
                              (list 24 45 51)
                              (list 30 40 50)))

(define (euler39 n)
  (for*/fold ([res 0]
              [res-sol '()]
              [res-l 0])
             ; a^2+b^2 = c^2 (1) If both a and b are even, c will also be even
             ; and p (the perimeter) will be even. If both a and b are odd,
             ; c will be even and p will be even. If one is even and the other
             ; is odd, c will be odd and p will again be even. Therefore, only
             ; even values of p need to be checked.
            ([p (in-range 2 (add1 n) 2)]
             [p-sol (in-value (sol p))]
             [p-l (length p-sol)]
             #:when (> p-l res-l))
            (values p p-sol p-l)))

(time (euler39 1000)) ; 500ms
