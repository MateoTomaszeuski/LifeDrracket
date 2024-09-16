#lang racket

(define (make-cell x y alive) 
  (list (cell=coordinates x y) alive)) 

(define (cell=coordinates x  y)
  (cons x y))
(define (x-coordinate c) (car (car c)))
(define (y-coordinate c) (cdr (car c)))
(define (alive? c) (cdr c))
(define cells '())

(define (append lst1 lst2)
  (foldr cons lst2 lst1))

(define (cell-exists cell-list c)
  (cond 
    [(member c cell-list) #t]
    [else #f]))

(define (get-alive-cells-around cell-list c)
  (define alive-cells '())
  (define top-left     (make-cell (- (x-coordinate c) 1)  (+ (y-coordinate c) 1) #t))
  (define top          (make-cell (x-coordinate c)        (+ (y-coordinate c) 1) #t))
  (define top-right    (make-cell (+ (x-coordinate c) 1)  (+ (y-coordinate c) 1) #t))
  (define left         (make-cell (- (x-coordinate c) 1)  (y-coordinate c) #t))
  (define right        (make-cell (+ (x-coordinate c) 1)  (y-coordinate c) #t))
  (define bottom-left  (make-cell (- (x-coordinate c) 1)  (- (y-coordinate c) 1) #t))
  (define bottom       (make-cell (x-coordinate c)        (- (y-coordinate c) 1) #t))
  (define bottom-right (make-cell (+ (x-coordinate c) 1)  (- (y-coordinate c) 1) #t))
  
  (when (cell-exists cell-list top-left)
    (set! alive-cells (append alive-cells (list top-left))))
  (when (cell-exists cell-list top)
    (set! alive-cells (append alive-cells (list top))))
  (when (cell-exists cell-list top-right)
    (set! alive-cells (append alive-cells (list top-right))))
  (when (cell-exists cell-list left)
    (set! alive-cells (append alive-cells (list left))))
  (when (cell-exists cell-list right)
    (set! alive-cells (append alive-cells (list right))))
  (when (cell-exists cell-list bottom-left)
    (set! alive-cells (append alive-cells (list bottom-left))))
  (when (cell-exists cell-list bottom)
    (set! alive-cells (append alive-cells (list bottom))))
  (when (cell-exists cell-list bottom-right)
    (set! alive-cells (append alive-cells (list bottom-right))))
  
  alive-cells)


(define c1(make-cell 1 1 #t))
(define c2(make-cell 2 1 #f))
(define c3(make-cell 2 2 #t))

(set! cells (append cells (list c1)))
(display cells)
(newline)
(set! cells (append cells (list c2)))
(display cells)
(newline)

(display(cell-exists cells c1))
(newline)
(display(cell-exists cells (make-cell 10 10 #f)))
(newline)
(display(cell-exists cells c2))
(newline)
(display(cell-exists cells (make-cell 1 1 #f)))
(newline)

(set! cells (append cells (list c3)))

(display(get-alive-cells-around cells c1))
(newline)

