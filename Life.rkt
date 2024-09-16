#lang racket

(define (make-cell x y alive) 
  (list (cell=coordinates x y) alive)) 
(define (cell=coordinates x  y)(cons x y))
(define (x-coordinate c) (car c))
(define (y-coordinate c) (cdr (car c)))

(define cells '())

(define (append lst1 lst2)
  (foldr cons lst2 lst1))

(define c1(make-cell 1 1 #t))
(define c2(make-cell 2 1 #f))

(define (cell-existis cell-list c)
  (cond 
    [(member c cell-list) #t]
    [else #f]))



(set! cells (append cells (list c1)))
(display cells)
(newline)
(set! cells (append cells (list c2)))
(display cells)
(newline)

(display(cell-existis cells c1))
(newline)
(display(cell-existis cells (make-cell 10 10 #f)))
(newline)
(display(cell-existis cells c2))
(newline)
