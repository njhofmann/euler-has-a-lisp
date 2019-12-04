#lang typed/racket
(require racket/math)
(require racket/dict)

(: integer->string (-> Integer String))
(define (integer->string int)
  (number->string int))

(: char->string (-> Char String))
(define (char->string char)
  (string char))

(: round-to-int (-> Real Integer))
(define (round-to-int num)
  (let ([result (exact-round num)])
    (if (integer? result)
        result
        0)))

(: char->string->integer (-> Char Integer))
(define (char->string->integer char)
  (let ([initial (string->number (char->string char))])
    (if (complex? initial)
        (round-to-int (real-part initial))
        0)))

(: split-number (-> Integer (Listof Integer)))
(define (split-number num)
  (map char->string->integer
       (string->list (integer->string num))))

(: is-palindrome-helper (-> (Listof Integer) (Listof Integer) Integer Boolean Boolean))
(define (is-palindrome-helper nums nums-stack count is-even)
  (cond
    [(and (not is-even) (equal? count 0)) (equal? (rest nums) nums-stack)]
    [(and is-even (equal? count 0)) (equal? nums nums-stack)]
    [else (is-palindrome-helper (rest nums) (cons (first nums) nums-stack) (sub1 count) is-even)]))

(: is-palindrome (-> Integer Boolean))
(define (is-palindrome nums)
  (let ([split-num (split-number nums)])
  (is-palindrome-helper split-num '() (floor (/ (length split-num) 2)) (even? (length split-num)))))

(: cross-product-range-helper (-> Integer Integer (Listof Integer) (Listof Integer) (Listof Integer)))
(define (cross-product-range-helper start cutoff to-apply acc)
  (if (> start cutoff)
      acc
      (cross-product-range-helper (add1 start) cutoff to-apply
                                  (append acc
                                          (map (lambda (#{x : Integer}) (* x start)) to-apply)))))


(: cross-product-range (-> Integer Integer (Listof Integer)))
(define (cross-product-range start end)
  (cross-product-range-helper start end (range start (add1 end)) '()))

(: max-palindrome (-> Integer Integer Integer))
(define (max-palindrome start end)
  (apply max
         (filter is-palindrome
                 (cross-product-range start end))))
                 
(check-eq? (max-palindrome 10 99) 9009)
(check-eq? (max-palindrome 100 999) 906609)
