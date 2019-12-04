#lang typed/racket
(require math)
(require typed/rackunit)

(: is-not-factor (-> Integer Integer Boolean))
(define (is-not-factor num factor)
  (not (is-factor num factor)))

(: is-factor (-> Integer Integer Boolean))
(define (is-factor num factor)
  (equal? (modulo num factor) 0))

(: find-primes-acc (-> Integer Integer (Listof Integer) (Listof Integer)))
(define (find-primes-acc num cur-val factors)
  (cond
    [(<= num cur-val) (cons num factors)]
    [(is-factor num cur-val) (let* ([times (exact-round (/ num cur-val))]
                                    [more-factors (find-primes-acc cur-val 2 factors)])
                               (find-primes-acc times 2 more-factors))]
    [else (find-primes-acc num (add1 cur-val) factors)]))

(: find-primes (-> Integer (Listof Integer)))
(define (find-primes num)
  (find-primes-acc num 2 '()))

(: sort-ints (-> (Listof Integer) (Listof Integer)))
(define (sort-ints ints)
  (sort ints <))

(: append-int-lists (-> (Listof Integer) (Listof Integer) (Listof Integer)))
(define (append-int-lists a b)
  (append a b))

(: union-factors-helper (-> (Listof Integer) (Listof Integer) (Listof Integer)))
(define (union-factors-helper potential prev)
  (cond
    [(empty? potential) prev]
    [(empty? prev) potential]
    [(equal? (first potential) (first prev)) (cons (first prev)
                                                   (union-factors-helper (rest potential)
                                                                         (rest prev)))]
    [else (cons (first prev) (union-factors-helper potential (rest prev)))]))
    

(: union-factors (-> (Listof Integer) (Listof Integer) (Listof Integer))) 
(define (union-factors potential-factors prev-factors)
  (union-factors-helper (sort-ints potential-factors) (sort-ints prev-factors)))

(: get-common-factors (-> Integer Integer (Listof Integer)))
(define (get-common-factors start end)
  (foldl union-factors '() (map find-primes (range start (add1 end)))))

(: smallest-divisible-number (-> Integer Integer Integer))
(define (smallest-divisible-number start end)
  (foldl * 1 (get-common-factors start end)))

(check-eq? (smallest-divisible-number 1 10) 2520)
(check-eq? (smallest-divisible-number 1 20) 232792560)