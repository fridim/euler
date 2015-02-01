#lang racket
#|
   Each character on a computer is assigned a unique code and the preferred
   standard is ASCII (American Standard Code for Information Interchange).
   For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

   A modern encryption method is to take a text file, convert the bytes to
   ASCII, then XOR each byte with a given value, taken from a secret key. The
   advantage with the XOR function is that using the same encryption key on
   the cipher text, restores the plain text; for example, 65 XOR 42 = 107,
   then 107 XOR 42 = 65.

   For unbreakable encryption, the key is the same length as the plain text
   message, and the key is made up of random bytes. The user would keep the
   encrypted message and the encryption key in different locations, and
   without both "halves", it is impossible to decrypt the message.

   Unfortunately, this method is impractical for most users, so the modified
   method is to use a password as a key. If the password is shorter than the
   message, which is likely, the key is repeated cyclically throughout the
   message. The balance for this method is using a sufficiently long password
   key for security, but short enough to be memorable.

   Your task has been made easy, as the encryption key consists of three
   lower case characters. Using [1]cipher1.txt, a file containing the
   encrypted ASCII codes, and the knowledge that the plain text must contain
   common English words, decrypt the message and find the sum of the ASCII
   values in the original text.


   Visible links
   1. cipher1.txt
   Answer: 68f891fe214e2bfa07c998ad5d0a390f
|#

(define s (file->string "cipher1.txt"))
(define s-final (substring s 0 (- (string-length s) 2))) ; because of \n\r
(define cipher-list
  (map string->number (string-split s-final ",")))

(define (apply-key k ls)
  (let loop ((left ls) (done '()))
    (cond ((null? left)
           done)
          ((< (length left)
              (length k))
           (append done (apply-key (take k (length left)) left)))
          (else
           (define part (take left (length k)))
           (define part-e (for/list ((a (in-list k))
                                     (b (in-list part)))
                            (bitwise-xor a b)))
           (define next-left (if (<= (length left)
                                     (length k))
                                 '()
                                 (list-tail left (length k))))
           (loop next-left
                 (append done part-e))))))

(define (list->ascii ls)
  (list->string (map integer->char ls)))

(for*/first ([a (in-range 97 123)]
             [b (in-range 97 123)]
             [c (in-range 97 123)]
             [clear (in-value (apply-key (list a b c) cipher-list))]
             [text (in-value (list->ascii clear))]
             #:when (for/and ((word (list "wh" "he" "was" "the" "with" "and")))
                      (regexp-match (string-append "(?i:" word ")")
                                    text)))
  (apply + clear))
