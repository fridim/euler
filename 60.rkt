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
         racket/set
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
  (for/set ([b (in-list ls)] #:when (linked? a b))
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
                           (set-union (point-connected-to old-p)
                                      (point-connected-to p)))
                    p)))
    (hash-set g (point-value p) new-p)))

; will complete connected-to points
(define (graph-complete g p)
  (define cp (point-connected-to p))
  (define v (point-value p))
  (for/fold ((res (graph-add g p)))
            ((cur (in-set cp)))
    (graph-add res (point cur (set v)))))

(define (mk-graph)
  ; todo: this would be more readable with for/fold
  (let loop ((pool primes) (res (hash)))
    (if (null? pool)
        res
        (let* ((i (car pool))
               (pi (pairs i (cdr pool))))
          (if (>= (set-count pi) 5)
              (loop (cdr pool) (graph-complete res (point i pi)))
              (loop (cdr pool) res))))))

(define (euler60)
  (define g (mk-graph))
  (define ls
    (for*/list ((cur (in-hash-values g))
                (cur-ct (in-value (point-connected-to cur)))
                (b (in-set cur-ct))
                (b-ct (in-value (set-add (point-connected-to (graph-get g b)) b)))
                (i (in-value (set-intersect b-ct (set-add cur-ct (point-value cur)))))
                #:when (>= (set-count i) 5)
                (c (in-set cur-ct))
                #:when (< b c)
                (c-ct (in-value (set-add (point-connected-to (graph-get g c)) c)))
                (j (in-value (set-intersect c-ct i)))
                #:when (>= (set-count j) 5)
                (d (in-set cur-ct))
                #:when (< c d)
                (d-ct (in-value (set-add (point-connected-to (graph-get g d)) d)))
                (k (in-value (set-intersect d-ct j)))
                #:when (>= (set-count k) 5)
                (e (in-set cur-ct))
                #:when (< d e)
                (e-ct (in-value (set-add (point-connected-to (graph-get g e)) e)))
                (l (in-value (set-intersect e-ct k)))
                #:when (= (set-count l) 5))
      (set->list l)))
  (apply min (map (lambda(i) (apply + i)) ls)))

(time (euler60)) ; 5300ms
