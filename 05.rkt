#lang racket

;; 2520 is the smallest number that can be divided by each of the numbers from
;; 1 to 10 without any remainder.
;; What is the smallest positive number that is evenly divisible by all of the
;; numbers from 1 to 20?


;; quick way, use already implemented lcm :
(apply lcm (range 1 11))
(apply lcm (range 1 21))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LONG way without lcm
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return list of prime-factors (with exposant)
;; ex: (prime-factors 8) -> '(2 2 2)
(define (prime-factors num)
  (let prime-factors-r ([n num] [i 2])
    (cond ((= n 1) '())
          ((= (modulo n i) 0)
           (cons i (prime-factors-r (quotient n i) i)))
          (else (prime-factors-r n (+ i 1))))))


;; thanks to ijp on #racket IRC channel
(define (longest-member2 ls)
  (if (null? ls) #f
      (for/fold ([current (car ls)]
                 [current-len (length (car ls))])
        ([next (cdr ls)])
        (let ([next-len (length next)])
          (if (< current-len next-len)
              (values next next-len)
              (values current current-len))))))

;; my recurcive version:
;; take a list of list and
;; return the longest list of the list.
(define (longest-member ls)
  (define (l-m ls current current-len)
    (if (null? ls)
        current
        (let ((next (car ls))
              (next-len (length (car ls))))
          (if (> next-len current-len)
              (l-m (cdr ls) next next-len)
              (l-m (cdr ls) current current-len)))))

  (if (null? ls)
      '()
      (l-m ls (car ls) (length (car ls)))))

(define (find-smallest-cm n)
  (define (flat-all ls)
    (apply * (map (curry apply *) ls)))

  (let ([pm-list (map prime-factors
                      (range 1 (+ n 1)))])
    (flat-all
     (for/list ([i (in-range 1 (+ n 1))])
       (longest-member (filter (negate empty?)
                               (map (lambda (l)
                                      (filter (curry = i) l))
                                    pm-list)))))))

(find-smallest-cm 10)
(find-smallest-cm 20)
