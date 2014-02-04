#lang racket/base


#|
An irrational decimal fraction is created by concatenating the positive
integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value of the
following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
|#

(require rackunit)

(define (gen n)
  (let ((limit (/ (* 1.45 n)
                  (/ (add1 (log n))
                     (log 10)))))
    (apply string-append
           (map number->string
                (for/list ([i (in-range 1 limit)])
                          i)))))

(check-true (< 10 (string-length (gen 10)) 20))
(check-true (< 100 (string-length (gen 100)) 200))
(check-true (< 1000 (string-length (gen 1000)) 2000))
(check-true (< 10000 (string-length (gen 10000)) 20000))
(check-true (< 100000 (string-length (gen 100000)) 200000))
(check-true (< 1000000 (string-length (gen 1000000)) 2000000))

(define (d n [f (gen n)])
  (- (char->integer (string-ref f (- n 1))) 48))

(check-equal? (d 1) 1)
(check-equal? (d 2) 2)
(check-equal? (d 11) 0)
(check-equal? (d 12) 1)
(check-equal? (d 13) 1)
(check-equal? (d 14) 1)
(check-equal? (d 15) 2)

(define my-f (time (gen 1000000))) ; 90ms

(* (d 10 my-f)
   (d 100 my-f)
   (d 1000 my-f)
   (d 10000 my-f)
   (d 100000 my-f)
   (d 1000000 my-f))
