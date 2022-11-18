#lang racket
;;##################Task1##################
(define (sum-of-digits a)
  (cond
    [(<= a 9) a]
    [else (+(remainder a 10)(sum-of-digits (quotient a 10)))]
  )
)
(define (plus-one a)
  ((+ a 1))) 
(define (count-specials k a b)
     (cond
       [(= a (+ b 1)) 0]
       [(and   (zero? (remainder a k))
               (zero? (remainder (sum-of-digits a) k)))
        (+ (count-specials k (+ 1 a) b)1)]
       [else (count-specials k (+ 1 a)b )]
     )
)

;;##################Task2##################
(define (num-len n)
  (cond
    [(< n 10) 1]
    [else (+ (num-len (quotient n 10)) 1)]
    )
  )
(define (rotate-number-with-keeping x n)
  (let ([length (num-len x)])
    (define (rotate-number x cut-len)
   (let  ([length2 (- length cut-len)])  
  (+(quotient x (expt 10 (- length2 1)))
 (*(remainder x (expt 10(- length2 1))) 10)
  )
   )
    )
    (+
     (*(quotient x (expt 10 (- length  n )))(expt 10 (- length n )))
     (rotate-number (remainder x (expt 10 (- length  n ))) n)
    )
   )
 )
(define (max-rot x)
  (let ([length (num-len x)]
        [n -1])
    (define (helper n x max-num)
    (cond
      [(= n (- length 1)) max-num]
      [(> x max-num) (helper (+ n 1) (rotate-number-with-keeping x (+ n 1)) x)]
      [else (helper (+ n 1) (rotate-number-with-keeping x (+ n 1)) max-num)]
    )
     )
  (helper n x x)
   ) 
 )
