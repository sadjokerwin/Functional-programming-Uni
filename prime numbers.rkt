#lang racket
(define (prime? n)
  (define (loop n c)
    {cond
      [(< n (* c 2)) #f]
      [(zero? (remainder n c)) #t]
      [else (loop n (+ c 1))]})
  (loop n 2))
(prime? 9)
      
