#lang racket/base

#| The number, 1406357289, is a 0 to 9 pandigital number because it is made up
of each of the digits 0 to 9 in some order, but it also has a rather interesting
sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note
the following:

    d2d3d4=406 is divisible by 2
    d3d4d5=063 is divisible by 3
    d4d5d6=635 is divisible by 5
    d5d6d7=357 is divisible by 7
    d6d7d8=572 is divisible by 11
    d7d8d9=728 is divisible by 13
    d8d9d10=289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property. |#

(require rackunit
         racket/list)

(define (match? l)
  (define (tonum a b c)
    (+ (* 100 (list-ref l (- a 1)))
       (* 10 (list-ref l (- b 1)))
       (list-ref l (- c 1))))
  (and (= 0 (modulo (list-ref l 3) 2))
       (= 0 (modulo (tonum 3 4 5) 3))
       (or (= (list-ref l 5) 0)
           (= (list-ref l 5) 5))
       (= 0 (modulo (tonum 5 6 7) 7))
       (= 0 (modulo (tonum 6 7 8) 11))
       (= 0 (modulo (tonum 7 8 9) 13))
       (= 0 (modulo (tonum 8 9 10) 17))))

(check-true (match? (list 1 4 0 6 3 5 7 2 8 9)))

(define (list->number ls)
  (string->number (list->string
                    (map (λ(n) (integer->char (+ n 48)))
                         ls))))
; taken from 41.rkt
(define (euler41)
  (apply +
         (map list->number
              (let loop ([l (range 0 10)]
                         [tail '()])
                (if (null? l)
                  (if (match? tail)
                    (list tail)
                    '())
                  (append-map (λ(x) (loop (remq x l) (cons x tail)))
                              l))))))

(time (euler41)) ; 1500 ms
