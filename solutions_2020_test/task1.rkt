#lang racket
(define (num-len a)
  (cond
    [(< a 10) 1]
    [else (+ (num-len (quotient a 10)) 1)]
  )
)
(define (reverse-num a)
  (define (helper acc a)
    (cond
      [(= a 0) acc]
      [else (helper (+ (* acc 10) (remainder a 10)) (quotient a 10))]
      )
    )
  (helper 0 a)
  )
(define (pow a n)
  (cond
    [(= n 1) a]
    [else (* (pow a (- n 1)) a)]))
(define (sum n p)
  (let ([len (num-len n)]) 
    (define (helper n p)
      (cond
        [(< n 10) (pow n p)]
        [else (+ (pow (remainder n 10) p) (helper (quotient n 10) (+ p 1)))]
     )
    )
    (helper (reverse-num n) p)
  )
)
(define (dig-pow n p)
  (define (helper counter)
    (cond
      [( > counter (/ (sum n p) n)) -1]
      [(eq? (* counter n) (sum n p)) counter]
      [else (helper (+ counter 1))]))
  (helper 1))
(dig-pow 695 2)
(dig-pow 46288 3)
    

