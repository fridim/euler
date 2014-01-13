#lang racket/base

#| We shall say that an n-digit number is pandigital if it makes use of all the
 | digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
 | through 5 pandigital.
 |
 | The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
 | multiplicand, multiplier, and product is 1 through 9 pandigital.
 |
 | Find the sum of all products whose multiplicand/multiplier/product identity
 | can be written as a 1 through 9 pandigital.
 | HINT: Some products can be obtained in more than one way so be sure to only
 | include it once in your sum.
 |#

(require racket/list
         rackunit)

(define (pandigital? str)
  (let ((ls (string->list str)))
    (and (= (length ls) 9)
         (equal? (sort ls char<?)
                 (string->list "123456789")))))

(define (digits n)
  (string-length (number->string n)))

;; Since we are looking for pandigital numbers, we know that the products must
;; all be 4-digit (1234 - 9876). The multiplicand and multiplier must either be
;; 1 x 4 or 2 x 3 digits.
(define (pan? multiplicand multiplier product)
  (and (<= 1234 product 9876)
       (or (and (= 1 (digits multiplicand))
                (= 4 (digits multiplier)))
           (and (= 2 (digits multiplicand))
                (= 3 (digits multiplier))))
       (pandigital? (string-append (number->string multiplicand)
                                   (number->string multiplier)
                                   (number->string product)))))

(check-equal? (pandigital? "123456798") #t)
(check-equal? (pandigital? "12456798") #f)
(check-equal? (pandigital? "1123456798") #f)
(check-equal? (pandigital? "123485679") #t)
(check-equal? (pandigital? "134586792") #t)
(check-equal? (pandigital? "391867254") #t)
(check-equal? (pan? 39 186 7254) #t)

(time (apply + (remove-duplicates
                 (for*/list ([a (in-range 1 100)]
                             [b (in-range (if (> a 9) 123 1234)
                                          (quotient 10000 (add1 a)))]
                             [p (in-value (* a b))]
                             #:when (pan? a b p))
                            p)))) ; 57 ms
