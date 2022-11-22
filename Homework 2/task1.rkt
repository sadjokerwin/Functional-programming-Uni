#lang racket
(define fs (list *
(λ (x y) (* x x x y))
(λ (x y) (+ x 1 y))
(λ (x y) (- x (+ 1 y)))
(λ (x y) (* x y 2))))
(define xs '(1 2 3 4 5))
(define (id x)
  x)
(define (pair-compose fs xs)
 (λ (y)
    (define (helper fs xs counter)
      (cond
       [(= counter 1) ((car fs) (car xs) (id y))]
        [(= counter 2) ((car fs) (car xs) ((cadr fs) (cadr xs) y))]
        [else (+ ((car fs) (car xs) ((cadr fs) (cadr xs) y))
               (helper (cddr fs) (cddr xs) (- counter 2)))]
        )
      )
    (helper fs xs (length fs))
    )
)
((pair-compose fs xs) 4)