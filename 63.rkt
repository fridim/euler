#lang racket/base

#| The 5-digit number, 16807=7^5, is also a fifth power.
Similarly, the 9-digit number, 134217728=8^9, is a ninth power.

How many n-digit positive integers exist which are also an nth power? |#

(require racket/list
         racket/set
         (only-in math exact-ceiling))


; to find the max(n), we look at the first n for which
; (number of digits of (9^n)) < n

(define (count-digits n)
  (if (<= n 1)
      1
      (abs (exact-ceiling (/ (log n) (log 10))))))

(define max-n
  (let loop ((n 1))
    (if (< (count-digits (expt 9 n)) n)
        n
        (loop (add1 n)))))

(time
 (for*/sum ([n (in-range 1 max-n)]
            [xn-min (in-value (expt 10 (- n 1)))]
            [xn-max (in-value (* 10 xn-min))]
            [x-min (in-value (floor (expt 10 (/ (- n 1) n))))]
            [x (in-range x-min 10)]
            [xn (in-value (expt x n))]
            #:when (and (<= xn-min xn)
                        (< xn xn-max)))
   1))  ; 0ms
