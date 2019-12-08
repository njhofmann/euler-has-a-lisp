#lang typed/racket
(require pfds/deque/bankers)

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

(: no-nums? (-> (Listof Integer) Boolean))
(define (no-nums? nums)
  (empty? nums))


;(: find-largest-adj-prod-acc (-> (Deque Integer) (Listof Integer) Integer Integer))
(define (find-largest-adj-prod-acc cur-digits future-digits largest-product)
  (if (no-nums? future-digits)
      largest-product
      (let* ([current-product (foldr * cur-digits)]
             [next-product (if (> current-product largest-product)
                               current-product
                               largest-product)])
        (find-largest-adj-prod-acc (enqueue (first future-digits) (init cur-digits))
                                   (rest future-digits) next-product))))

;(: find-largest-adj-prod-setup (-> (Deque Integer) (Listof Integer) Integer Integer))
(define (find-largest-adj-prod-setup start-digits digits to-add)
  (if (equal? to-add 0)
      (find-largest-adj-prod-acc start-digits digits -1)
      (find-largest-adj-prod-setup (enqueue (first digits) start-digits)
                                   (rest digits)
                                   (sub1 to-add))))

;(: find-largest-adj-prod (-> Integer Integer Integer))
(define (find-largest-adj-prod digit subset-size)
  (find-largest-adj-prod-setup (empty Integer) (split-number digit) subset-size))