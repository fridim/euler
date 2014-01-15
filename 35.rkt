#lang racket/base

#| The number, 197, is called a circular prime because all rotations of
the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
73, 79, and 97.

How many circular primes are there below one million? |#

(require rackunit
         (only-in math prime? next-prime))

(define (number->list n)
  (map (lambda (i) (- (char->integer i) 48)) (string->list (number->string n))))

(define (list->number ls)
  (string->number (list->string
                    (map (lambda (n) (integer->char (+ n 48)))
                         ls))))
(define (next-rotation p)
  (let ((lsp (number->list p)))
    (list->number (append (cdr lsp) (list (car lsp))))))

(define (all-rotations p)
  (let helper ((p-current p) (res (list p)))
    (let ((p-next (next-rotation p-current)))
      (if (= p-next p)
        res
        (helper p-next (cons p-next res))))))

(check-equal? (all-rotations 197) (list 719 971 197 ))
(check-equal? (all-rotations 1972) (list 2197 7219 9721 1972 ))
(check-equal? (all-rotations 79) (list 97 79))

(define (circular? p)
  (for/and ([r (in-list (all-rotations p))])
           (prime? r)))

(check-equal? (circular? 197) #t)
(check-equal? (circular? 79) #t)
(check-equal? (circular? 3) #t)

(define (circular-primes maxn)
  (let helper ([current-p 7] [res (list 5 3 2)])
    (cond ((>= current-p maxn)
           (reverse res))
          ((and (null?
                  (filter (lambda (n) (or (= n 0) (= n 2)
                                          (= n 4) (= n 8)
                                          (= n 6) (= n 5)))
                          (number->list current-p)))
                (circular? current-p))
           (helper (next-prime current-p) (cons current-p res)))
          (else
            (helper (next-prime current-p) res)))))

(check-equal? (circular-primes 100)
              (list 2 3 5 7 11 13 17 31 37 71 73 79 97))

(time (length (circular-primes 1000000)))
