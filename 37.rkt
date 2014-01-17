#lang racket/base

#| The number 3797 has an interesting property. Being prime itself, it is
possible to continuously remove digits from left to right, and remain prime at
each stage: 3797, 797, 97, and 7. Similarly we can work from right to left:
3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to
right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes. |#

(require rackunit
         (only-in racket/list last)
         (only-in math prime? next-prime))

(define (truncatable-prime? p)
  (and (prime? p)
       (let ((digits (floor (add1 (/ (log p) (log 10))))))
         (for/and ([i (in-range 1 digits)])
                  (and (prime? (floor (/ p (* (expt 10 i)))))
                       (prime? (floor (modulo p (* (expt 10 i))))))))))

(check-equal? (truncatable-prime? 3797) #t)

(define (next-p n)
  (let* ((np (+ n 2))
         (lsp (string->list (number->string np))))
    ; it can't start with a 1 or a 9 (they are not prime digits)
    (if (and (not (eq? (car lsp) #\1))
             (not (eq? (car lsp) #\9))
             (not (eq? (last lsp) #\9))
             (not (eq? (last lsp) #\1))
             (or (< np 100)
                 (null? (filter
                          (lambda (n) (or (eq? n #\4)
                                          (eq? n #\6)
                                          (eq? n #\8)
                                          (eq? n #\0)))
                          lsp))))
      np
      (next-p np))))

(define (euler37)
  (let recc ([res '()]
             [current-p 11])
    (cond ((= (length res) 11)
           (apply + res))
          ((truncatable-prime? current-p)
           (recc (cons current-p res) (next-p current-p)))
          (else
            (recc res (next-p current-p))))))

(time (euler37)) ; 250ms
