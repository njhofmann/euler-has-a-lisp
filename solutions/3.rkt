#lang racket
(require math)
(require rackunit)

(define (is-not-factor  num factor)
  (not (is-factor num factor)))

(define (is-factor num factor)
  (equal? (modulo num factor) 0))

(define (find-primes-acc num cur-val factors)
  (cond
    [(<= num cur-val) (cons num factors)]
    [(is-factor num cur-val) (let* ([times (/ num cur-val)]
                                   [more-factors (find-primes-acc cur-val 2 factors)])
                               (find-primes-acc times 2 more-factors))]
    [else (find-primes-acc num (add1 cur-val) factors)]))

(define (find-primes num)
  (find-primes-acc num 2 '()))

(define (max-in-list values)
  (apply max values))

(define (get-largest-prime-factor num)
  (max-in-list (find-primes num)))

(check-eq? (get-largest-prime-factor 100) 5)
(check-eq? (get-largest-prime-factor 71) 71)
(check-eq? (get-largest-prime-factor 600851475143) 6857)
              