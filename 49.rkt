#lang racket/base

#| The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
increases by 3330, is unusual in two ways: (i) each of the three terms are
prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this
sequence? |#

(require racket/string
         racket/list
         (only-in math next-primes))

(define (permutation? a b)
  (define (sort-digits n)
    (sort (string->list (number->string n)) char<?))

  (equal? (sort-digits a)
          (sort-digits b)))

(require rackunit)

(check-true (permutation? 1487 4817))
(check-true (permutation? 8147 4817))

(define (constant-increase ls)
  (let loop ([li ls])
    (if (< (length li) 3)
      #f
      (let ((p1 (first li))
            (p2 (second li))
            (p3 (third li)))
        (if (= (- p2 p1)
               (- p3 p2))
          (list p1 p2 p3)
          (loop (cdr li)))))))

(check-equal? (constant-increase (list 1487 4817 8147))
              (list 1487 4817 8147))

(define (euler49)
  (let loop ([ls (reverse (next-primes 1001 1061))])
    (if (< (length ls) 3)
      #f
      (let* ([p (car ls)]
             [p-permutations (sort (filter (lambda (n) (permutation? n p)) ls) <)]
             [ci (constant-increase p-permutations)])
        (if ci
          ci
          (loop (remq* p-permutations (cdr ls))))))))

(time (euler49)) ; 77ms
