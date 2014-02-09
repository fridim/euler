#lang racket/base

#| The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its
alphabetical position and adding these values we form a word value. For example,
the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a
triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
containing nearly two-thousand common English words, how many are triangle
words? |#

;; t = 1/2 * n * (n + 1)
;; t = 1/2 * n^2 + n / 2
;; t = (n ^ 2 + n) / 2
;; 2t = n^2 + n
;; n^2 + n - 2t = 0
;; => discriminant and root to find the inverse function :
;; n = (Sqr(1 + 8t) - 1) / 2
(define (triangle-number? t)
  (integer? (/ (- (sqrt (+ 1 (* 8 t))) 1) 2)))

(require rackunit)

(check-equal? (triangle-number? 1) #t)
(check-equal? (triangle-number? 3) #t)
(check-equal? (triangle-number? 6) #t)
(check-equal? (triangle-number? 21) #t)
(check-equal? (triangle-number? 22) #f)
(check-equal? (triangle-number? 55) #t)

(define (sum-word w)
  (for/sum ([i (in-list (string->list w))])
           (- (char->integer i) 64)))

(check-equal? (sum-word "SKY") 55)

(define (triangle-word? w)
  (triangle-number? (sum-word w)))

(check-true (triangle-word? "SKY"))

(require racket/file
         racket/string)

(define (euler42)
  (length
    (filter triangle-word?
            (string-split (string-replace (file->string "words.txt") "\"" "") ","))))

(time (euler42)) ; 10 ms
