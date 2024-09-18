#lang racket

(define (make-cell x y) 
  (cons x y))

(define (x-coordinate c) (car c))
(define (y-coordinate c) (cdr c))
(define cells '())

(define (not-null? c)
  (not (eq? c 'null)))

(define (append lst1 lst2)
  (foldr cons lst2 lst1))

(define (cell-exists cell-list c)
  (member c cell-list))

(define (get-alive-cells-around cell-list c)
  (define alive-cells '())
  (define (add-if-exists cell)
    (when (cell-exists cell-list cell)
      (set! alive-cells (append alive-cells (list cell)))))
  (define top-left     (make-cell (- (x-coordinate c) 1)  (+ (y-coordinate c) 1)))
  (define top          (make-cell (x-coordinate c)        (+ (y-coordinate c) 1)))
  (define top-right    (make-cell (+ (x-coordinate c) 1)  (+ (y-coordinate c) 1)))
  (define left         (make-cell (- (x-coordinate c) 1)  (y-coordinate c)))
  (define right        (make-cell (+ (x-coordinate c) 1)  (y-coordinate c)))
  (define bottom-left  (make-cell (- (x-coordinate c) 1)  (- (y-coordinate c) 1)))
  (define bottom       (make-cell (x-coordinate c)        (- (y-coordinate c) 1)))
  (define bottom-right (make-cell (+ (x-coordinate c) 1)  (- (y-coordinate c) 1)))

  (for ([cell (list top-left top top-right left right bottom-left bottom bottom-right)])
    (add-if-exists cell))
  
  (length alive-cells))

(define (alive-or-kill cell-list)
  (map (lambda (c)
         (define alive-count (get-alive-cells-around cell-list c))
         (cond
          [(or (= alive-count 2) (= alive-count 3)) c]
          [else 'null]))
       cell-list))

(define (get-surrounding-cells c)
  (list
   (make-cell (- (x-coordinate c) 1) (+ (y-coordinate c) 1))
   (make-cell (x-coordinate c) (+ (y-coordinate c) 1))
   (make-cell (+ (x-coordinate c) 1) (+ (y-coordinate c) 1))
   (make-cell (- (x-coordinate c) 1) (y-coordinate c))
   (make-cell (+ (x-coordinate c) 1) (y-coordinate c))
   (make-cell (- (x-coordinate c) 1) (- (y-coordinate c) 1))
   (make-cell (x-coordinate c) (- (y-coordinate c) 1))
   (make-cell (+ (x-coordinate c) 1) (- (y-coordinate c) 1))))

(define (cell-birth cell-list)
  (define surrounding-cells '())
  (for-each
    (lambda (cell)
      (set! surrounding-cells (append surrounding-cells (get-surrounding-cells cell))))
    cell-list)
  (define unique-surrounding-cells (remove-duplicates surrounding-cells))
  (define (surrounding-alive-count c)
    (define surrounding (get-surrounding-cells c))
    (length (filter (lambda (neighbor) (cell-exists cell-list neighbor)) surrounding)))
  (filter (lambda (c)
            (= (surrounding-alive-count c) 3))
          unique-surrounding-cells))


(define (generation cell-list)
 (append (filter not-null? (alive-or-kill cell-list)) (cell-birth cell-list))
)


; Initialize cells
(define c1 (make-cell 0 0))
(define c2 (make-cell 0 1))
(define c3 (make-cell 0 2))
(define c4 (make-cell 1 0))
(define c5 (make-cell 2 1))
 (set! cells (append cells (list c1)))
 (set! cells (append cells (list c2)))
 (set! cells (append cells (list c3)))
 (set! cells (append cells (list c4)))
 (set! cells (append cells (list c5)))

 (display "Display Initial Board:")
 (newline)
 (display cells)
 (newline)
 
(let loop ((times 0))
(if (= times 100000) (display "done")
  (begin 
  ;;; (newline)
;;;  (display "Display New Board: ")(display times)
  ;;; (newline)
 (set! cells (generation cells))
  ;;; (display cells)
  ;;; (newline)
 (loop (+ times 1)) )))
 (display cells)
  (newline)