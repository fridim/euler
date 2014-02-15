;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
;; that the 6th prime is 13.
;; What is the 10 001st prime number?

#lang racket/base

(define (prime? n)
  (and (not (= (modulo n 2) 0))
       (for/and ([i (in-range 3 (add1 (sqrt n)) 2)])
                (not (= 0 (modulo n i))))))

(define (prime-at n)
  (let loop ([found 2] [cur 3])
    (if (prime? cur)
      (if (= found n) cur
        (loop (add1 found) (+ cur 2)))
      (loop found (+ cur 2)))))

(time (prime-at 10001)) ; 103 ms
