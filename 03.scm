#lang racket/base

;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?

(define (prime-factors n i)
  (cond ((= n 1) '())
        ((= (modulo n i) 0) (cons i (prime-factors (/ n i) (+ i 1))))
        (else (prime-factors n (+ i 1)))))

(apply max (prime-factors 13195 1))
(apply max (prime-factors 600851475143 1))
