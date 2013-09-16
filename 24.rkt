#lang racket/base
(require racket/set
         racket/list
         rackunit)

#|
A permutation is an ordered arrangement of objects. For example, 3124 is one
possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are
listed numerically or alphabetically, we call it lexicographic order. The
lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5,
6, 7, 8 and 9?
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Brute force   (not working)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(for*/list ((i 3) (j 3) (k 3)
                  #:when (not (or (= i j) (= i k) (= k j))))
           (list i j k))

(define (integer->list n)
  (map (lambda (c) (- (char->integer c) 48))
       (string->list (number->string n))))

(define (all-digits-different? i)
  (let ((ls (integer->list i)))
    (= (length ls) (set-count (list->set ls)))))

; this is brute force. Would take hours.
; with limit = 1000, it takes 10 secs, and grows linearly.
(define (euler24-brute limit)
  (let helper ([i 0123456789] [acc 1])
    (cond [(all-digits-different? i)
           (if (= acc limit) i
             (helper (+ 1 i) (+ 1 acc)))]
          [else
            (helper (+ 1 i) acc)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Narayana Pandita's method
; http://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The following algorithm finds the next lexicographic permutation in order:
;-->
; Find the largest index k such that a[k] < a[k + 1]. If no such index
; exists, the permutation is the last permutation.
; Find the largest index l such that a[k] < a[l].
; Swap the value of a[k] with that of a[l].
; Reverse the sequence from a[k + 1] up to and including the final element a[n].
;<--

(define (next-permutation ls)
  (let helper ([k (- (length ls) 2)]
               [l (- (length ls) 1)])
    (if (< k 0)
      ls
      (let ((lsk  (list-ref ls k))
            (lsl  (list-ref ls l))
            (lsk1 (list-ref ls (+ k 1))))
        (if (< lsk lsk1)
          (if (< lsk lsl)
            (let ((swapped (map (lambda (n)
                                  (cond ((= n lsk) lsl)
                                        ((= n lsl) lsk)
                                        (else n)))
                                ls)))
              (append (for/list ([i (in-range (+ k 1))]) (list-ref swapped i))
                      (reverse (list-tail swapped (+ k 1)))))
            (helper k (- l 1)))
          (helper (- k 1) l))))))

;; Tests
(check-equal? (next-permutation '(1 2 3 4))
              '(1 2 4 3))
(check-equal? (next-permutation '(1 2 4 3))
              '(1 3 2 4))

(define (euler24 limit)
  (let helper ([i 1] [ls (range 10)])
    (if (= i limit)
      ls
      (helper (+ i 1) (next-permutation ls)))))

(time (euler24 1000000)) ; 800 ms
