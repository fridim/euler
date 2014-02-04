#lang racket/base

#| We shall say that an n-digit number is pandigital if it makes use of all the
 | digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
 | also prime.
 |
 | What is the largest n-digit pandigital prime that exists? |#

(require rackunit
         racket/list
         (only-in math prime?)
         (only-in math/number-theory permutations))

(define (number->vector n)
  (list->vector
    (map (lambda (i) (- (char->integer i) 48))
         (string->list (number->string n)))))

(define (vector->number v)
  (string->number (list->string
                    (map (lambda (n) (integer->char (+ n 48)))
                         (vector->list v)))))

(define (list->number ls)
  (string->number (list->string
                    (map (lambda (n) (integer->char (+ n 48)))
                         ls))))

(define (perms n)
  (sort
    (map list->number
       (let ((l (range 1 (add1 n))))
         (let loop ([l l] [tail '()])
           (if (null? l) (list tail)
             (append-map (λ(x) (loop (remq x l) (cons x tail))) l))))) >))


(check-equal? (length (perms 4)) (permutations 4 4))

; Nine-digit numbers don't have to be checked
; (1+2+3+4+5+6+7+8+9=45 => always dividable by 3)
; Same for eight-digit numbers (1+2+3+4+5+6+7+8=36 => always dividable by 3)
(time
  (for*/last ([i (in-range 7 0 -1)]
              [p (in-list (perms i))]
              #:final (prime? p))
             p)) ; 7652413  (20ms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; try another way (one permutation after the other)

(define (number->list n)
  (map (lambda (i) (- (char->integer i) 48))
       (string->list (number->string n))))

; Modified Narayana Pandita's method
; http://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
(define (next-permutation n)
  (list->number
    (let ((ls (number->list n)))
      (let helper ([k (- (length ls) 2)]
                   [l (- (length ls) 1)])
        (if (< k 0)
          ls
          (let ((lsk  (list-ref ls k))
                (lsl  (list-ref ls l))
                (lsk1 (list-ref ls (+ k 1))))
            (if (> lsk lsk1)
              (if (> lsk lsl)
                (let ((swapped (map (lambda (n)
                                      (cond ((= n lsk) lsl)
                                            ((= n lsl) lsk)
                                            (else n)))
                                    ls)))
                  (append (for/list ([i (in-range (+ k 1))]) (list-ref swapped i))
                          (reverse (list-tail swapped (+ k 1)))))
                (helper k (- l 1)))
              (helper (- k 1) l))))))))


(define (euler41)
  (let helper ((cur 7654321))
    (if (prime? cur)
      cur
      (helper (next-permutation cur)))))

(time (euler41))  ; 0ms ! \o/
