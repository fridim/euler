#lang racket/base

#| In the card game poker, a hand consists of five cards and are ranked, from
lowest to highest, in the following way:

    High Card: Highest value card.
    One Pair: Two cards of the same value.
    Two Pairs: Two different pairs.
    Three of a Kind: Three cards of the same value.
    Straight: All cards are consecutive values.
    Flush: All cards of the same suit.
    Full House: Three of a kind and a pair.
    Four of a Kind: Four cards of the same value.
    Straight Flush: All cards are consecutive values of same suit.
    Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest
value wins; for example, a pair of eights beats a pair of fives (see example 1
below). But if two ranks tie, for example, both players have a pair of queens,
then highest cards in each hand are compared (see example 4 below); if the
highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:
Hand	 	Player 1	 	Player 2	 	Winner
1	 	5H 5C 6S 7S KD Pair of Fives 2C 3S 8S 8D TD Pair of Eights Player 2
2	 	5D 8C 9S JS AC Highest card Ace 2C 5C 7D 8S QH Highest card Queen Player 1
3	 	2D 9C AS AH AC Three Aces 3D 6D 7D TD QD Flush with Diamonds Player 2
4	 	4D 6S 9H QH QC Pair of Queens Highest card Nine 3D 6D 7H QD QS Pair of
                                         Queens Highest card Seven Player 1
5	 	2H 2D 4C 4D 4S Full House With Three Fours 3C 3D 3S 9S 9D Full House
                                              with Three Threes Player 1

The file, poker.txt, contains one-thousand random hands dealt to two players.
Each line of the file contains ten cards (separated by a single space): the
first five are Player 1's cards and the last five are Player 2's cards. You can
assume that all hands are valid (no invalid characters or repeated cards), each
player's hand is in no specific order, and in each hand there is a clear winner.

How many hands does Player 1 win? |#

(require rackunit
         racket/list
         racket/string
         racket/set)

(struct hand (values suits) #:transparent)

(define (string->hand s)
  (define sp (string-split s " "))
  (define v (map
              (λ(n) (let ((sn (substring n 0 1)))
                         (case sn
                           [("T") ":"]
                           [("J") ";"]
                           [("Q") "<"]
                           [("K") "="]
                           [("A") ">"]
                           [else sn])))
              sp))
  (define suits (map (λ(n) (substring n 1 2)) sp))
  (hand v suits))

(define (consecutive? h)
  (define sorted (map (λ(s) (char->integer (string-ref s 0)))
                      (sort (hand-values h) string<?)))
  (let loop ((f (first sorted))
             (r (cdr sorted)))
    (if (null? r)
      #t
      (if (= (add1 f)
             (car r))
        (loop (add1 f)
              (cdr r))
        #f))))

(define (straight? h)
  (consecutive? h))

(check-equal? (straight? (string->hand "2H 2D 4C 4D 4S")) #f)
(check-equal? (straight? (string->hand "2H 4D 3C 6D 5S")) #t)
(check-equal? (straight? (string->hand "JH KD QC AD TS")) #t)
(check-equal? (straight? (string->hand "9S KD TC JD QS")) #t)
(check-equal? (straight? (string->hand "JH 9D QC AD TS")) #f)

(define (nth? h n)
  (define hv (hand-values h))
  (for/or ((i (in-list hv)))
          (= n (count (λ(n) (string=? i n)) hv))))

(define (four? h)
  (nth? h 4))

(check-equal? (four? (string->hand "QS KD QC QD QH")) #t)
(check-equal? (four? (string->hand "KS KD QC QD QH")) #f)
(check-equal? (four? (string->hand "AS KD QC QD QH")) #f)

(define (flush? h)
  (= (set-count (list->set (hand-suits h)))
     1))

(check-equal? (flush? (string->hand "9S KS TS JS QS")) #t)
(check-equal? (flush? (string->hand "9H KS TS JS QS")) #f)

(define (three? h)
  (nth? h 3))

(check-equal? (three? (string->hand "TS KS TS TS QS")) #t)
(check-equal? (three? (string->hand "9H KS TS JS QS")) #f)
(check-equal? (three? (string->hand "9H 9S TS 9S 9S")) #f)

(define (hpair? h)
  (nth? h 2))

(check-equal? (hpair? (string->hand "TS KS TS TS QS")) #f)
(check-equal? (hpair? (string->hand "9H KS TS JS JS")) #t)
(check-equal? (hpair? (string->hand "9H 9S TS 9S 9S")) #f)

(define (two-pair? h)
  (define hv (hand-values h))
  (and (for/or ((i (in-list hv)))
               (= 2 (count (λ(n) (string=? n i)) hv)))
       (= 3 (set-count (list->set hv)))))

(check-equal? (two-pair? (string->hand "TS KS TS TS QS")) #f)
(check-equal? (two-pair? (string->hand "TH KS TS JH JS")) #t)
(check-equal? (two-pair? (string->hand "9H 9S TS 9S 9S")) #f)

(define (val h)
  (string-join
    (sort
      (hand-values h)
      string>?)
    ""))

(check-equal? (val (string->hand "TH KS TS JH JS")) "=;;::")
(check-equal? (val (string->hand "9H 9S TS 9S 9S")) ":9999")
(check-equal? (val (string->hand "9H 2S TS 8S 3S")) ":9832")

(define (val-nth h n)
  (define hv (hand-values h))
  (for/first ((i (in-list hv))
              #:when (= n (count (λ(n) (string=? i n)) hv)))
             i))

(define (val-four h)
  (val-nth h 4))

(define (val-three h)
  (val-nth h 3))

(define (val-pair h)
  (val-nth h 2))

(define (val-two-pair h)
  (define hv (hand-values h))
  (string-join (sort
                 (for/list ((i (in-list hv))
                            #:when (= 2 (count (λ(n) (string=? i n)) hv)))
                           i)
                 string>?)
               ""))

;; I'm going to use the alphabetical sort : V is for value
;    High Card        VVVVV
;    One Pair         zVVVV
;    Two Pairs        zzVVV
;    Three of a Kind  zzzVVV
;    Straight         zzzzV
;    Flush            zzzzzV
;    Full House       zzzzzzVV
;    Four of a Kind   zzzzzzzV
;    Straight Flush   zzzzzzzzV
;    Royal Flush      zzzzzzzzV
(define (hand->string h)
  (cond ((flush? h)
         (if (straight? h)
           (string-append "zzzzzzzz" (val h))
           (string-append "zzzzz" (val h))))
        ((four? h)
         (string-append "zzzzzzz" (val-four h) (val h)))
        ((straight? h)
         (string-append "zzzz" (val h)))
        ((three? h)
         (if (hpair? h)
           (string-append "zzzzzz" (val-three h) (val-pair h))
           (string-append "zzz" (val-three h) (val h))))
        ((hpair? h)
         (if (two-pair? h)
           (string-append "zz" (val-two-pair h) (val h))
           (string-append "z" (val-pair h) (val h))))
        (else
          (val h))))

(check-equal? (hand->string (string->hand "TH KS TS JH JS")) "zz;;::=;;::")
(check-equal? (hand->string (string->hand "9H 9S TS 9S 9S")) "zzzzzzz9:9999")
(check-equal? (hand->string (string->hand "9H 2S TS 8S 3S")) ":9832")

(define (euler54)
  (for*/sum
    ((line (in-lines (open-input-file "poker.txt")))
     (h1 (in-value (string->hand (substring line 0 14))))
     (h2 (in-value (string->hand (substring line 15 29))))
     #:when (string>? (hand->string h1) (hand->string h2)))
    1))

(time (euler54)) ; 60 ms
