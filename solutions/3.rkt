
(define (not-evenly-divides num factor)
  (not (evenly-divides num factor)))

(define (evenly-divides num factor)
  (integer? (/ num factor)))


(define (get-primes cutoff)
  (if (empty? cutoff)
    '()
    (cons (first cutoff) 
          (get-primes (filter (lambda (x) 
                                (not-evenly-divides x (first cutoff)))
                              (rest cutoff))))))


(define (get-primes-less-than cutoff)
  (get-primes (range 2 (add1 cutoff))))

(define (max-in-list values)
  (apply max values))

(define (get-largest-prime-factor num)
  (max-in-list (filter (lambda (x) (evenly-divides num x)) (get-primes-less-than num))))


(get-largest-prime-factor 600851475143)
              