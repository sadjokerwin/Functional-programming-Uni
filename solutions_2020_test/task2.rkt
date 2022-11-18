#lang racket
(define (neg-list xs)
  (define (helper xs n acc)
    (cond
      [(zero? n) acc]
      [(< (car xs) 0) (cons (car xs) (helper (cdr xs) (- n 1)  acc))]
      [else (helper (cdr xs) (- n 1)  acc )]
      ))
  (helper xs (length xs) '()))
(neg-list '(1 2 3 4 -5 6 7 -2 -1 0))
