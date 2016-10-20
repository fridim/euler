#lang racket/base

;; https://projecteuler.net/problem=65

(require racket/list racket/string rackunit)

(define (seq n)
  (take
   (cons 2 (for/fold
               ((buf (list)))
               ((i (in-range (add1 (floor (/ n 3))))))
             (append buf (list 1 (* 2 (add1 i)) 1))))
   n))

(define (fractionize lst)
  (if (= (length lst) 1)
      (car lst)
      (+ (car lst) (/ 1 (fractionize (cdr lst))))))

(define (sum-digits n)
  (apply + (filter number?
                   (map string->number
                        (string-split
                         (number->string n)
                         "")))))

(define (euler65 n)
  (sum-digits (numerator (fractionize (seq n)))))

(check-equal? (euler65 10) 17)

(time (euler65 100)) ; cpu time: 0ms !
