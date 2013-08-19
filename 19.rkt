#lang racket

#|
You are given the following information, but you may prefer to do some
research for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, but not on a century
    unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1
Jan 1901 to 31 Dec 2000)?
|#

(define (months-of year)
  (list 31 (if (is-leap? year) 29 28) 31 30 31 30 31 31 30 31 30 31 0))

(define (is-leap? year)
  (or (and (= (modulo year 100) 0)
           (= (modulo year 400) 0))
      (= (modulo year 4) 0)))

(define (euler19)
  (let count-sundays ([day 6] ;; first sunday on 1901
                      [month 1]
                      [year 1901]
                      [months (months-of 1900)]
                      [sum 0])
    (let ([days (list-ref months (- month 1))])
      (cond
        [(> month 12)
         (count-sundays day 1 (+ 1 year) (months-of (+ 1 year)) sum)]
        [(> day days)
         (count-sundays (- day days) (+ 1 month) year months sum)]
        [(= year 2001)
         sum]
        [(= day 1)
         (count-sundays (+ day 7) month year months (+ 1 sum))]
        [else
          (count-sundays (+ day 7) month year months sum)]))))

(time (euler19))
