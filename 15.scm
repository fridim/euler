;; http://projecteuler.net/problem=15

#lang racket/base

;; this one is too slow to get the answer :(
(define (euler15-naive n)
  (define (helper i j)
    (cond [(and (< i n) (< j n))
           ;; this is a new path: +1
           (+ 1 (helper (+ i 1) j) (helper i (+ j 1)))]
          ;; continue a path : 0
          [else 0]))
  (+ 1 (helper 0 0)))

;; I ran the previous euler15-naive "on paper" and drew it. Then the 
;; Pascal triangle showed up : 
;; Let consider the next intersection (i,j), and P(i,j) the number
;; of paths passing this intersection. Then :
;; P(i,j) = P(i-1,j) + P(i, j-1)
;; The following procedure is an update of the previous euler15-naive with
;; the formula :
(define (euler15 n)
  (define (get i j v)
    (list-ref v (+ i (* n j))))
  (let helper ([i 0] [j 0] [v '()])
    (cond [(= i n)
           (helper 0 (+ 1 j) v)] ;; next row
          [(or (= i 0) (= j 0))
           (helper (+ i 1) j (append v '(1)))] ;; one more path
          [(and (< i n) (< j n))
           (helper (+ i 1) j
                   (append v (list (+ (get (- i 1) j v)
                                      (get i (- j 1) v)))))]
          [else
            (+ 1 (apply + v))])))

(time (euler15 20)) ;; < 2ms  \o/

;; this is also :
(require math)
(+ 1 (binomial 40 20))
