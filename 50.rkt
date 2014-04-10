#lang racket/base

#| The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below
one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime,
contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most
consecutive primes? |#

(require (only-in math next-prime prime?)
         racket/list)

(define (first-primes limit)
  (reverse
    (let loop ([cur 3] [ls (list 2)] [sum 2])
      (if (> sum limit)
        ls
        (loop (next-prime cur) (cons cur ls) (+ sum cur))))))

(define (euler50 limit)
  (define (prime-sum ls cur-max)
    (cond ((< (length ls) cur-max) '())
          ((prime? (apply + ls)) ls)
          (else (prime-sum (drop-right ls 1) cur-max))))

  (define (longest l1 l2)
    (let ((len-l1 (length l1))
          (len-l2 (length l2)))
      (cond ((> len-l1 len-l2)
             l1)
            ((= len-l1 len-l2)
             (if (> (apply + l1) (apply + l2)) l1 l2))
            (else
              l2))))

  (apply +
         (let loop ([ls (first-primes limit)] [cur-max 0])
           (if (< (length ls) cur-max)
             '()
             (let* ((result (prime-sum (drop-right ls 1) cur-max))
                    (result-next (loop (cdr ls)
                                       (max (length result) cur-max))))
               (longest result result-next))))))

(require rackunit)

(check-equal? (euler50 100) 41)
(check-equal? (euler50 1000) 953)

(time (euler50 1000000)) ; < 1 ms
