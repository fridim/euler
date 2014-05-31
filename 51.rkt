#lang racket/base

#| By replacing the 1st digit of the 2-digit number *3, it turns out that six of
the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit
number is the first example having seven primes among the ten generated numbers,
yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
Consequently 56003, being the first member of this family, is the smallest prime
with this property.

Find the smallest prime which, by replacing part of the number (not necessarily
adjacent digits) with the same digit, is part of an eight prime value family. |#

(require rackunit
         racket/list
         (only-in math prime? next-prime exact-ceiling))

(define (count-digits n)
  (if (<= n 1)
    1
    (exact-ceiling (/ (log n) (log 10)))))

(check-equal? (count-digits 123456) 6)
(check-equal? (count-digits 1234567) 7)
(check-equal? (count-digits 123456789012345678901234567890) 30)
(check-equal? (count-digits 1) 1)

(define (set-digit n pos val)
  (define cd (count-digits n))
  (define mul (expt 10 (- cd (add1 pos))))
  (define valp (* val mul))
  (define right (modulo n mul))
  (define left (* mul 10 (quotient n (* 10 mul))))
  (+ right valp left))

(check-equal? (set-digit 100203 2 2) 102203)
(check-equal? (set-digit 100203 1 2) 120203)
(check-equal? (set-digit 902203 5 9) 902209)
(check-equal? (set-digit 123456789 5 9) 123459789)
(check-equal? (set-digit 123456789 8 0) 123456780)
(check-equal? (set-digit 123456789 0 9) 923456789)
(check-equal? (set-digit 99 1 1) 91)
(check-equal? (set-digit 99 0 1) 19)
(check-equal? (set-digit 1 0 9) 9)

;; It will need 3 digit-changes because of the divibility by 3
;; find all generated primes by changing 3 digits with [0-9]
(define (longest-pserie p)
  (define cp (count-digits p))
  (define m
    (for*/list ([pos1 cp]
                ; pos1 2 3 must all be ≠
                [pos2 (in-range (add1 pos1) (- cp 2))]
                ; last digit cannot be replaced: we need 8 primes
                [pos3 (in-range (add1 pos2) (- cp 1))])
               (for/list ([i 10]
                          #:when (not (and (= pos1 0) (= i 0)))
                          [a (in-value (set-digit
                                         (set-digit
                                           (set-digit p pos1 i)
                                           pos2 i)
                                         pos3 i))]
                          #:when (prime? a))
                         a)))
  (first
    (sort m
          (λ(lsa lsb) (<= (length lsb)
                          (length lsa))))))

(define (euler51)
  (let loop ([cur 100001]) ; it must be 6-digit or more
    (let ((longest (longest-pserie cur)))
      (if (= 8 (length longest))
        (apply min longest)
        (loop (next-prime cur))))))

(time (euler51)) ; 450 ms
