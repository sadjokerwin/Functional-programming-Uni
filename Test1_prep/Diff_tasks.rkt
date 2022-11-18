#lang racket
(define (my-max-div a)
  (define (helper a max cnt)
    (cond
      [(= cnt (quotient a 2)) cnt]
      [(zero? (remainder a cnt)) (helper a cnt (+ cnt 1))]
      [else (helper a max (+ cnt 1))]))
  (helper a 1 1)
  )
;;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (prime? a)
  (define (prime-helper a div)
    (cond
      [(or (= a 1) (= a 2)) #t]
      [(= div (+ (quotient a 2) 1)) #t]
      [(zero? (remainder a div)) #f]
      [else (prime-helper a (+ div 1))]))
  (prime-helper a 2))
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (reverse-num a)
  (define (helper a acc)
  (cond
    [(= a 0) acc]
    [else (helper (quotient a 10) (+ (* acc 10) (remainder a 10)))]
    ))
 ( helper a 0))
(define (palyndrome? a)
  (= a (reverse-num a)))
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (pow x n)
    (cond
     [(zero? n) 1]
     [else (* (pow x (- n 1)) x)]
     )
  )
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (descending-order? a)
  (cond
    [(< a 10) #t]
    [(> (remainder a 10) (remainder (quotient a 10) 10))#f]
   [else (descending-order? (quotient a 10))]
   )
)
                 
(define (sum-numbers a b)
  (cond
    [(= a (+ b 1)) 0]
    [(descending-order? a) (+ (sum-numbers (+ a 1) b) a)]
    [else (sum-numbers (+ a 1) b)]))
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (bigger-els a xs)
  (cond
    [(empty? xs) 0]
    [(< a (car xs)) (+ 1 (bigger-els a (cdr xs)))]
    [else (bigger-els a (cdr xs))]
  )
 )
(define (num-bigger-elements lst)
  (define (helper xs)
    (cond
      [(empty? xs) '()]
      [else (cons
             (cons (car xs) (bigger-els (car xs) lst))
             (helper (cdr xs)))]
     )
  )
  (helper lst)
  )
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (func f g n)
  (lambda (x) 
    (define (helper n)
      (cond
        [(= 1 n) (f x)]
        [(odd? n) (f (helper (- n 1)))]
        [else (g (helper (- n 1)))]
    )
  )
  (helper n)
  )
)

((func (lambda (x) (+ x 1))
(lambda (x) (* x 2)) 2) 2)
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
