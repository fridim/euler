;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;; Find the sum of all the primes below two million.

#lang racket

(define (get-primes n)
  (let get-primes2 ([l (range 2 n)])
    (if (null? l)
      '()
      (let* ([i (car l)]
             [lsp (filter (lambda (n) (not (= (modulo n i) 0))) l)])
        (if (> (* i i) n)
          lsp
          (cons i (get-primes2 lsp)))))))

(time (apply + (get-primes 2000000)))
