#|
A unit fraction contains 1 in the numerator. The decimal representation of the
unit fractions with denominators 2 to 10 are given:

    1/2	= 	0.5
    1/3	= 	0.(3)
    1/4	= 	0.25
    1/5	= 	0.2
    1/6	= 	0.1(6)
    1/7	= 	0.(142857)
    1/8	= 	0.125
    1/9	= 	0.(1)
    1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in
its decimal fraction part.
|#

#lang racket/base

(require rackunit)

(define (reccuring-cycle n)
  (let helper ([r 1] [done (list)])
    (if (= r 0) 0
      (let ((cycle-members (member r (reverse done))))
        (if cycle-members ; found cycle !
          (length cycle-members)
          (if (= r 1)
            (helper (* r 10) (cons r done))
            (helper (* 10 (remainder r n)) (cons r done))))))))

(check-equal? (reccuring-cycle 3) 1)
(check-equal? (reccuring-cycle 6) 1)
(check-equal? (reccuring-cycle 7) 6)
(check-equal? (reccuring-cycle 8) 0)
(check-equal? (reccuring-cycle 9) 1)
(check-equal? (reccuring-cycle 23) 22)
(check-equal? (reccuring-cycle 28) 6)
(check-equal? (reccuring-cycle 41) 5)
(check-equal? (reccuring-cycle 49) 42)
(check-equal? (reccuring-cycle 97) 96)
(check-equal? (reccuring-cycle 119) 48)

(define (euler26 limit)
  (let find-max ([current limit] [final 7] [final-cycles 6])
    ; The period of 1/k for integer k is always ≤ k − 1.
    (if (or (< (- current 1) final-cycles)
            (= current 1))
      (values final final-cycles)
      (let ((n-cycles (reccuring-cycle current)))
        (if (> n-cycles final-cycles)
          (find-max (- current 1) current n-cycles)
          (find-max (- current 1) final final-cycles))))))

(time (euler26 1000))
