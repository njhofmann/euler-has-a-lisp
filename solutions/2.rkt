#lang racket
(require rackunit)

(define (sum-of-even-fib-acc a b acc cutoff)
  (let* ([new-b (+ a b)]
         [new-acc (if (even? new-b) 
                      (+ acc new-b)
                       acc)])
         (if (>= new-b cutoff) 
             acc
             (sum-of-even-fib-acc b new-b new-acc cutoff))))

(define (sum-of-even-fibs cutoff)
  (sum-of-even-fib-acc 1 2 2 cutoff))

(check-eq? (sum-of-even-fibs 10) 10)
(check-eq? (sum-of-even-fibs 4000000) 4613732)
