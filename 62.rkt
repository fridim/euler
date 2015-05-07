#lang racket/base

#| The cube, 41063625 (345³), can be permuted to produce two other cubes:
56623104 (384³) and 66430125 (405³). In fact, 41063625 is the smallest cube
which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits
are cube. |#

(require rackunit)
(require racket/list)

(define (number->list n)
  (let loop ((i n))
    (if (< i 10)
        (list i)
        (cons (remainder i 10) (loop (quotient i 10))))))
    
(define (euler62 number-of-permutations)
  ; inc takes an Immutable Hash and a Number. Returns an Immutable Hash OR a Number
  ; this proc 'inc' increments the value of h[n]
  ; if the number of permutations is reached, then it returns the min of the
  ; collected permutations
  (define (inc h n)
    (define key (list->bytes (sort (number->list n) <)))
    (define current-value (hash-ref h key (list 0 999999999999999)))
    (define next-value (list (add1 (first current-value))
                             (min (second current-value) n)))
    (if (= (first next-value) number-of-permutations)
        (second next-value)
        (hash-set h key next-value)))
  
  (let loop ((cur 100) (h (hash)))
    (if (hash? h)
        (loop (add1 cur) (inc h (expt cur 3)))
        h)))

(check-equal? 41063625 (euler62 3))
(time (euler62 5)) ; 40ms
