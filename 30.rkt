#lang racket/base

#|
Surprisingly there are only three numbers that can be written as the sum of
fourth powers of their digits:

    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4

As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers
of their digits.
|#

(require racket/list)

; The maximum value for one digit is 9^5 = 59049
; So the maximum value for a 6 and 7 digit number would be :
; (* 6 59049) = 354294
; (* 7 59049) = 413343
; => Numbers we are looking for are 6 digits or less.

; so let's brute force.
(define (euler30)
  (- (for*/fold ((sum 0))
                ([a (in-range 10)]
                 [b (in-range 10)]
                 [c (in-range 10)]
                 [d (in-range 10)]
                 [e (in-range 10)]
                 [f (in-range 10)]
                 [oneway (in-value (+ (* 100000 a)
                                      (* 10000 b)
                                      (* 1000 c)
                                      (* 100 d)
                                      (* 10 e)
                                      f))]
                 [otherway (in-value (+ (expt a 5)
                                        (expt b 5)
                                        (expt c 5)
                                        (expt d 5)
                                        (expt e 5)
                                        (expt f 5)))]
                 #:when (= oneway otherway))
                (+ sum oneway))
     ; 1 is not a sum
     1))

(time (euler30)) ; 350 ms
