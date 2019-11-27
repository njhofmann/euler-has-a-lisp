(define (fizz-mults end)
  (sum
    (filter (lambda (x) (or (equal? (modulo x 5) 0)
                            (equal? (modulo x 3) 0)))
            (stream->list (in-range end)))))

 