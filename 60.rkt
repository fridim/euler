#lang racket/base

#| The primes 3, 7, 109, and 673, are quite remarkable. By taking any two
primes and concatenating them in any order the result will always be
prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The
sum of these four primes, 792, represents the lowest sum for a set of four
primes with this property.

Find the lowest sum for a set of five primes for which any two primes
concatenate to produce another prime. |#

(custodian-limit-memory (current-custodian) (* 1024 1024 900))

(require (only-in math prime? next-primes))

(require racket/list
         rackunit)

(define (concat a b)
  (string->number (string-append (number->string a)
                                 (number->string b))))

(module+ test
  (check-equal? (concat 12 232) 12232)
  (check-equal? (concat 1 232) 1232)
  (check-equal? (concat 200 1) 2001))

(define limit 1100) ;

; remove 2, can't be included in the set, won't concat into a prime
(define primes (next-primes 2 limit))

(define (linked? p1 p2)
  (and (prime? (concat p1 p2))
       (prime? (concat p2 p1))))

(module+ test
  (check-true (linked? 3 7))
  (check-true (linked? 109 673))
  (check-true (linked? 109 3)))

(define (pairs a ls)
  (for/list ([b (in-list ls)] #:when (linked? a b))
    b))

(struct point (value connected-to) #:transparent)

(define (graph-get g p)
  (hash-ref g p #f))

(define (graph-del g p)
  (hash-remove g p))


(define (graph-add g p)
  (let* ((old-p (graph-get g (point-value p)))
         (new-p (if old-p
                    (point (point-value p)
                           (remove-duplicates (append (point-connected-to old-p)
                                                      (point-connected-to p))))
                    p)))
    (hash-set g (point-value p) new-p)))

; will complete connected-to points
(define (graph-complete g p)
  (define cp (point-connected-to p))
  (define v (point-value p))
  (let loop ((ls cp) (res (graph-add g p)))
    (if (null? ls)
        res
        (let ((cur (car ls)))
          (loop (cdr ls)
                (graph-add res
                           (point cur (list v))))))))

(define (mk-graph)
  (let loop ((pool primes) (res (hash)))
    (if (null? pool)
        res
        (let* ((i (car pool))
               (pi (pairs i (cdr pool))))
          (if (>= (length pi) 5)
              (loop (cdr pool) (graph-complete res (point i pi)))
              (loop (cdr pool) res))))))


;; We need to find 5 vertices  connected to each other
; - only look for vertices with at least 5 connections (mk-graph)
; - at-least-5: filter vertices -> for each vertex v, keep v if:
;    - v connections have at least 5 connections with other vertices connected to v
(define (graph-filter g)
  ; we are looking only at point with at least 5 connections
  (for*/list ((cur (in-hash-values g))
              (cur-ct (in-value (point-connected-to cur)))
              (cur-v (in-value (point-value cur)))
              (at-least-5 (in-value (filter (lambda(p)
                                              (>= (length
                                                   (filter (lambda(pp) (member pp cur-ct))
                                                           (point-connected-to (graph-get g p))))
                                                  5))
                                            cur-ct)))
              #:when (>= (length at-least-5) 5))
    (cons cur-v cur-ct)))

(define (all-connected? g ls)
  (for*/and ((a (in-list ls))
             (b (in-list ls))
             #:when (not (= a b))
             (pa (in-value (graph-get g a))))
    (if (member b (point-connected-to pa)) #t #f)))

(module+ test
  (check-true (all-connected? (mk-graph) (list  3 7 109 673))))

(define (euler60)
  (define g (mk-graph))
  (define candidats (reverse (graph-filter g)))
  ; for*/first instead of for*/list : assume it's the first found  (lucky it is)
  (for*/first  ((p (in-list candidats))
               (lp (in-range (length p)))
               (p1 (in-range lp))
               (p2 (in-range (add1 p1) lp))
               (p3 (in-range (add1 p2) lp))
               (p4 (in-range (add1 p3) lp))
               (p5 (in-range (add1 p4) lp))
               #:when (< p1 p2 p3 p4 p5)
               (perm (in-value (list (list-ref p p1)
                                     (list-ref p p2)
                                     (list-ref p p3)
                                     (list-ref p p4)
                                     (list-ref p p5))))
               #:when (all-connected? g perm))
    perm))
;(apply min (map (lambda(i) (apply + i)) matches)))
