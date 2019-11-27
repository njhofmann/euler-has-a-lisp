#lang racket
(require math)
(require rackunit)

(define (is-factor num factor)
  (equal? (modulo num factor) 0))


(define (fizz-mults cutoff)
  (sum (filter (lambda (x) (or (is-factor x 5)
                               (is-factor x 3)))
               (range cutoff))))


(check-eq? (fizz-mults 10) 23)
(check-eq? (fizz-mults 1000) 233168)



 