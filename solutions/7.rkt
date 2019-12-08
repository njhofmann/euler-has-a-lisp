#lang typed/racket


(: has-any-factor (-> Integer (Listof Integer) Boolean))
(define (has-any-factor value factors)
  (cond
    [(empty? factors) #f]
    [(equal? (modulo value (first factors)) 0) #t]
    [else (has-any-factor value (rest factors))]))


(: find-nth-prime (-> Integer (Listof Integer) Integer Integer Integer))
(define (find-nth-prime value primes prime-count target-count)
  (cond
    [(has-any-factor value primes) (find-nth-prime (add1 value) primes prime-count target-count)]
    [(equal? (add1 prime-count) target-count) value]
    [else (find-nth-prime (add1 value) (cons value primes) (add1 prime-count) target-count)]))

(: find-10001st-prime (-> Integer))
(define (find-10001st-prime)
  (find-nth-prime 2 '(2) 1 10001))

(equal? (find-10001st-prime) 104743)