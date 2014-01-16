#lang racket/base

#| The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in
base 10 and base 2.

(Please note that the palindromic number, in either base, may not include
leading zeros.) |#

(require rackunit)

(define (pal-b? n base)
  (let helper ((reversed 0)
               (current n))
    (cond ((= current 0)
           (= n reversed))
          (else
            (helper (+ (* base reversed) (modulo current base))
                    (quotient current base))))))

(check-equal? (pal-b? 585 10) #t)
(check-equal? (pal-b? 585 2) #t)
(check-equal? (pal-b? 11911 10) #t)
(check-equal? (pal-b? 2 2) #f)

(time (for/sum ([i (in-range 1 1000000)]
                ;; even numbers can't be palindrome in base 2 (~ 1...0)
                #:when (and (odd? i)
                            (pal-b? i 10)
                            (pal-b? i 2)))
               i)) ; 97ms
