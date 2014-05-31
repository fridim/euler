#lang racket/base

#| It can be seen that the number, 125874, and its double, 251748, contain
exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain
the same digits. |# 

(define (sort-digits n)
  (sort (string->list (number->string n)) char<?))

(define (permutation? a b)
  (equal? (sort-digits a)
          (sort-digits b)))

(define (euler52)
  ; I assume it's higher thant the example (as always…)
  (let loop ([cur 125874])
    (if (for/and ([i (in-range 2 7)])
                 (permutation? cur (* i cur)))
      cur
      (loop (add1 cur)))))

(time (euler52)) ; 40 ms
