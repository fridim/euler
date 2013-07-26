#lang racket/base 

;; A palindromic number reads the same both ways. The largest palindrome made
;; from the product of two 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit numbers.

(define (is-palindrome? n)
  (let ([slist (string->list (number->string n))])
    (equal? slist (reverse slist))))

;; return the largest palindrome made from the product of two n-digit numbers.
(define (find-palindrome n)
  (let ([limit (- (expt 10 n) 1)])
    (define (find-palindrome-recc a b)
      (cond
        [(= a 0)
         '()]
        [(= b 0)
         (find-palindrome-recc (- a 1) (- a 1))]
        [(is-palindrome? (* a b))
         (cons (* a b) (find-palindrome-recc (- a 1) (- a 1)))]
        [else 
          (find-palindrome-recc a (- b 1))]))
    (apply max (find-palindrome-recc limit limit))))

(find-palindrome 2)
(find-palindrome 3) 
