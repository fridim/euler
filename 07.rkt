;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
;; that the 6th prime is 13.
;; What is the 10 001st prime number?

#lang racket

(define (is-prime? n)
  (define (is-prime2? n limit i)
    (if (> i limit)
      #t
      (if (= (modulo n i) 0)
        #f
        (is-prime2? n limit (+ i 1)))))
  (is-prime2? n (quotient n 2) 2))

(define (get-first-primes n)
  (define (g-f-p prime-list prime-list-len acc)
    (if (= prime-list-len n)
      prime-list
      (if (is-prime? acc)
        (g-f-p (cons acc prime-list) (add1 prime-list-len) (+ 2 acc))
        (g-f-p prime-list prime-list-len (+ 2 acc)))))
  (g-f-p '(2 3 5) 3 7))

(define (get-first-primes2 n)
  (let g-f-p ([found 1] [acc 3])
    (if (= found n)
      '()
      (if (is-prime? acc)
        (cons acc (g-f-p (add1 found) (+ 2 acc)))
        (g-f-p found (+ 2 acc))))))

(time (car (get-first-primes 10001)))
(time (last (get-first-primes2 10001)))
