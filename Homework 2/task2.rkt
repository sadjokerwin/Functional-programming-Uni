#lang racket
(define xs '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (20 . 1)))
;;the algorithm is that we always try to cut the tree left, so it doesn't hinder any further progress
(define (woodcutters xs)
  (define (helper xs prev current taken-spaces cut-trees)
   (cond
     [(null? (cdr xs)) (+ cut-trees 1)]
     [(= 0 cut-trees) (helper (cdr xs) current (cadr xs)  0 (+ cut-trees 1))]
     [(< (cdr current) (- (- (car current) (car prev)) taken-spaces)) (helper (cdr xs) current (cadr xs) 0 (+ cut-trees 1))]
     ;;(height current < ((x of current - x of prev)-taken-spaces from the downed tree)
     [(< (cdr current) (- (caar (cdr xs)) (car current))) (helper (cdr xs) current (cadr xs) (cdr current) (+ cut-trees 1))]
     ;;(height current < ((x of next - x of current)- 0 taken-spaces from the downed tree(because we cut right
     ;;and we can't have any fallen trees on the right as we iter from left to right)
     [else (helper (cdr xs) current (cadr xs) 0 (+ cut-trees 0))]
     )
    )
  (helper xs (car xs) (car xs) 0 0))


(woodcutters '((1 . 7) (3 . 11) (6 . 12) (7 . 6) (8 . 5) (9 . 11)
(15 . 3) (16 . 10) (22 . 2) (23 . 3) (25 . 7) (27 . 3) (34 . 5)
(35 . 10) (37 . 3) (39 . 4) (40 . 5) (41 . 1) (44 . 1) (47 . 7)
(48 . 11) (50 . 6) (52 . 5) (57 . 2) (58 . 7) (60 . 4) (62 . 1)
(67 . 3) (68 . 12) (69 . 8) (70 . 1) (71 . 5) (72 . 5)
(73 . 6) (74 . 4) )) 




;(car xs)