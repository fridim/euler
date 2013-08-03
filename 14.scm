;; The following iterative sequence is defined for the set of positive integers:
;; 
;; n → n/2 (n is even)
;; n → 3n + 1 (n is odd)
;; 
;; Using the rule above and starting with 13, we generate the following
;; sequence:
;; 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
;; 
;; It can be seen that this sequence (starting at 13 and finishing at 1)
;; contains 10 terms. Although it has not been proved yet (Collatz Problem), it
;; is thought that all starting numbers finish at 1.
;; 
;; Which starting number, under one million, produces the longest chain?
;; 
;; NOTE: Once the chain starts the terms are allowed to go above one million.

#lang racket/base

(define (collatz n)
  (cond [(= n 1) 0]
        [(even? n)
         (let ([next (/ n 2)])
           (+ 1 (collatz next)))]
        [else
          (let ([next (add1 (* 3 n))])
            (+ 1 (collatz next)))]))

(define (euler14)
  (let helper ([i 2] [max-n 0] [r 2])
    (if (= i 999999)
      r
      (let ([len (collatz i)])
        (if (> len max-n)
          (helper (+ i 1) len i)
          (helper (+ i 1) max-n r))))))

(time (euler14)) ;; brute force, ~ 2.5secs on my laptop
