;; If the numbers 1 to 5 are written out in words: one, two, three, four, five,
;; then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out
;; in words, how many letters would be used?
;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
;; forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
;; letters. The use of "and" when writing out numbers is in compliance with
;; British usage.

#lang racket/base

(define (letters n)
  (cond
    [(= n 1) "one"]
    [(= n 2) "two"]
    [(= n 3) "three"]
    [(= n 4) "four"]
    [(= n 5) "five"]
    [(= n 6) "six"]
    [(= n 7) "seven"]
    [(= n 8) "eight"]
    [(= n 9) "nine"]
    [(= n 10) "ten"]
    [(= n 11) "eleven"]
    [(= n 12) "twelve"]
    [(= n 13) "thirteen"]
    [(= n 14) "fourteen"]
    [(= n 15) "fifteen"]
    [(= n 16) "sixteen"]
    [(= n 17) "seventeen"]
    [(= n 18) "eighteen"]
    [(= n 19) "nineteen"]
    [(= n 20) "twenty"]
    [(= n 30) "thirty"]
    [(= n 40) "forty"]
    [(= n 50) "fifty"]
    [(= n 60) "sixty"]
    [(= n 70) "seventy"]
    [(= n 80) "eighty"]
    [(= n 90) "ninety"]
    [(= n 1000) "onethousand"]
    [(< n 100)
     (let ([q (quotient n 10)]
           [r (remainder n 10)])
       (string-append (letters (* 10 q))
                      (letters r)))]
    [(> n 99)
     (let ([q (quotient n 100)]
           [r (remainder n 100)])
       (string-append (letters q)
                      "hundred"
                      (if (= r 0)
                        ""
                        (string-append "and" (letters r)))))]))

(string-length
  (apply string-append
         (for/list ([i (in-range 1 1001)])
                   (letters i))))
