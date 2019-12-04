#lang typed/racket
(require rackunit)

(: sum (-> (Listof Integer) Integer))
(define (sum nums)
  (foldl + 0 nums))

(: square (-> Integer Integer))
(define (square num)
  (expt num 2))

(: square-of-sum (-> (Listof Integer) Integer))
(define (square-of-sum nums)
  (square (sum nums)))

(: sum-of-squares (-> (Listof Integer) Integer))
(define (sum-of-squares nums)
  (sum (map square nums)))

(: sum-square-diff (-> Integer Integer Integer))
(define (sum-square-diff start end)
  (let ([nums (range start (add1 end))])
      (- (square-of-sum nums) (sum-of-squares nums))))

(check-eq? (sum-square-diff 1 100) 25164150)