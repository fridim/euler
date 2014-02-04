#lang racket/base

#| Take the number 192 and multiply it by each of 1, 2, and 3:

    192 × 1 = 192
    192 × 2 = 384
    192 × 3 = 576

By concatenating each product we get the 1 to 9 pandigital, 192384576. We will
call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and
5,giving the pandigital, 918273645, which is the concatenated product of 9 and
(1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
concatenated product of an integer with (1,2, ... , n) where n > 1? |#

(require rackunit
         racket/list)

(define (pandigital? str)
  (let ((ls (string->list str)))
    (and (= (length ls) 9)
         (equal? (sort ls char<?)
                 (string->list "123456789")))))

(define (concat-products n ps)
  (apply string-append
         (map (lambda (i)
                (number->string (* n i)))
              ps)))

(define (pandigital-product? n ps)
  (pandigital? (concat-products n ps)))

(check-equal? (pandigital-product? 192 (list 1 2 3)) #t)
(check-equal? (pandigital-product? 9 (list 1 2 3 4 5)) #t)

(time
  (apply max
         (for*/list ([n (in-range 10000 0 -1)]
                     [i (in-range 2 11)]
                     #:when (pandigital-product? n (range 1 i)))
                    (string->number
                      (concat-products n (range 1 i)))))) ; 140ms
