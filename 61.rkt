#lang racket/base

#| Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are
all figurate (polygonal) numbers and are generated by the following formulae:
Triangle 	  	P3,n=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
Square 	  	P4,n=n2 	  	1, 4, 9, 16, 25, ...
Pentagonal 	  	P5,n=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...
Hexagonal 	  	P6,n=n(2n−1) 	  	1, 6, 15, 28, 45, ...
Heptagonal 	  	P7,n=n(5n−3)/2 	  	1, 7, 18, 34, 55, ...
Octagonal 	  	P8,n=n(3n−2) 	  	1, 8, 21, 40, 65, ...

The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three
interesting properties.

    1/ The set is cyclic, in that the last two digits of each number is the first
two digits of the next number (including the last number with the first).

    2/ Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and
pentagonal (P5,44=2882), is represented by a different number in the set.

    3/ This is the only set of 4-digit numbers with this property.

Find the sum of the only ordered set of six cyclic 4-digit numbers for which
each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and
octagonal, is represented by a different number in the set. |#

(define (p3 n) (/ (* (+ n 1) n) 2))
(define (p4 n) (* n n))
(define (p5 n) (/ (* n (- (* 3 n) 1)) 2))
(define (p6 n) (* n (- (* 2 n) 1)))
(define (p7 n) (/ (* n (- (* 5 n) 3)) 2))
(define (p8 n) (* n (- (* 3 n) 2)))

(define (gen p mi ma)
  (for/list ((n (in-range mi ma)))
    (p n)))

;; P3,45 < 1000  and  P3,142 > 10000
;; and so on...
(define p3-candidates (gen p3 46 142))
(define p4-candidates (gen p4 32 101))
(define p5-candidates (gen p5 26 82))
(define p6-candidates (gen p6 23 71))
(define p7-candidates (gen p7 21 64))
(define p8-candidates (gen p8 19 59))
(define candidates (append p4-candidates
                           p5-candidates
                           p6-candidates
                           p7-candidates
                           p8-candidates))

(define (cyclic? a b)
  (= (modulo a 100)
     (quotient b 100)))

(define (got-one? ls c)
  (for/or ((i (in-list ls)))
    (member i c)))

(time
 (apply +
        (for*/first ((i3 (in-list p3-candidates))
                     (i4 (in-list candidates))
                     #:when (cyclic? i3 i4)
                     (i5 (in-list candidates))
                     #:when (cyclic? i4 i5)
                     (i6 (in-list candidates))
                     #:when (cyclic? i5 i6)
                     (i7 (in-list candidates))
                     #:when (cyclic? i6 i7)
                     (i8 (in-list candidates))
                     #:when (cyclic? i7 i8)
                     #:when (cyclic? i8 i3)
                     (ls (in-value (list i4 i5 i6 i7 i8)))
                     #:when (and (got-one? ls p4-candidates)
                                 (got-one? ls p5-candidates)
                                 (got-one? ls p6-candidates)
                                 (got-one? ls p7-candidates)
                                 (got-one? ls p8-candidates)))
          (cons i3 ls)))) ;; 50 ms
