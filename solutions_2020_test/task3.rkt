#lang racket
(define (num-of-div a)
  (define (helper acc counter)
    (cond
      [(= counter a) (+ acc 1)]
      [(zero? (remainder a counter)) (helper (+ acc 1) (+ counter 1))]
      [else  (helper acc (+ counter 1))]))
  (helper 0 1))
(define (max-div a b)
  (define (helper max counter num)
    (cond
      [(= a counter) num]
      [(> (num-of-div counter) max) (helper (num-of-div counter) (- counter 1) counter)]
      [else (helper max (- counter 1) num)]))
  (helper 0 (- (* b 2) 1) 0))
(define (best-pair a b)
  (if (odd? (max-div a b))
      (cons (quotient (max-div a b) 2) (+ (quotient (max-div a b) 2) 1))
      (cons (- (/ (max-div a b) 2) 2) (+ (/ (max-div a b) 2) 2))
   )
 )
(best-pair 10 20)
(best-pair 10 30)
(best-pair 10 40)
(best-pair 10 50)
