#lang racket/base

#| The fraction 49/98 is a curious fraction, as an inexperienced mathematician
in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is
correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than
one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find
the value of the denominator. |#

(time
  (denominator
    (apply *
           (for*/list ([a (in-range 1 10)]
                       [b (in-range (add1 a) 10)]
                       [x (in-range 1 10)]
                       [f (in-value (/ a b))]
                       #:when (or (= (/ (+ (* 10 a) x)
                                        (+ (* 10 b) x))
                                     f)
                                  (= (/ (+ (* 10 a) x)
                                        (+ (* 10 x) b))
                                     f)
                                  (= (/ (+ (* 10 x) a)
                                        (+ (* 10 x) b))
                                     f)
                                  (= (/ (+ (* 10 x) a)
                                        (+ (* 10 b) x))
                                     f)))
                      f))))
