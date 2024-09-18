#lang racket
 
; define cell
(define (make-cell coordinates)
   coordinates)
 
; selects the cell's coordinates
(define (cell-coordinates cell)
  cell)
 
(define (make-point x y)
  (cons x y))
 
; get x value from pair of coordinates
(define (get-x-value coordinates)
  (car coordinates))
; get y value from pair of coordinates
(define (get-y-value coordinates)
  (cdr coordinates))
 
 
(define (get-x-from-cell cell)
  (get-x-value (cell-coordinates cell)))
 
(define (get-y-from-cell cell)
  (get-y-value (cell-coordinates cell)))
 
(define (compare-coordinates cell1 cell2) 
  (define cell1x (get-x-from-cell cell1))
  (define cell1y (get-y-from-cell cell1))
 
  (define cell2x (get-x-from-cell cell2))
  (define cell2y (get-y-from-cell cell2))
 
  (and (= cell1x cell2x) (= cell1y cell2y)) ; simplified this  
)
 
; list of live cells
(define live-cells '())
 
; global list of cells? do we have to pass it in?
(define (update-live-cells new-live-cells)
  (set! live-cells new-live-cells)
)
 
; create new live cells
(define (create-new-live-cells)
  (define (rules-per-cell-helper cell)
    (let ((count (count-live-neighbors cell)))
      (cond ((and (member cell live-cells) (or (= count 2) (= count 3))) cell) ; if alive with 2-3 neighbors, stay alive
            ((and (not (member cell live-cells)) (= count 3)) (make-cell (cell-coordinates cell))) ; if dead with 3 neighbors, come alive
            (else #f))))
  (filter (lambda (cell) (not (false? (rules-per-cell-helper cell))))
          (generate-potential-cells))) ; apply the rules to every potential cell
 
 
(define (count-live-neighbors cell)
  (define (count-helper neighbors count) ; helper function to loop through neighbors and see if they're alive
    (if (null? neighbors) ; base case (end of list)
        count
        (let ((neighbor (car neighbors)))
          (let ((cell-to-check (make-cell neighbor)))
          (let ((new-count (if (member cell-to-check live-cells)
                              (+ count 1)
                              count))) ; if neighbor is alive, increment count; otherwise, count stays the same
              (count-helper (cdr neighbors) new-count)))))) ; recursive call
          (count-helper (make-neighbor-list cell) 0)) ; calling the helper function with the list of neighbors, count starting at 0
 
 
; generate a list of all cells that are alive or could become alive in the next iteration
(define (generate-potential-cells)
  (define (append-if-not-in-list item list)
    (if (member item list)
      list
      (cons item list))) ; if it's not a member of the list, add it
  (define (generate-all-potential-cells cells result) ; recursive function similar to count-live-neighbors
    (if (null? cells) ; base case
      result
        (let ((current (car cells)))
          (let ((neighbors (make-neighbor-list current)))
            (generate-all-potential-cells (cdr cells)
                      (foldl append-if-not-in-list result neighbors)))))) ; foldl lets us loop over every item, applying the function
  (generate-all-potential-cells live-cells '()) ; call function on live cells, starting with an empty list
)
 
(define (make-neighbor-list cell)
  (define current-x (get-x-from-cell cell))
  (define current-y (get-y-from-cell cell))
  (list (make-point (- current-x 1) (+ current-y 1)) ; upper-left
        (make-point current-x (+ current-y 1))      ; upper
        (make-point (+ current-x 1) (+ current-y 1)) ; upper-right
        (make-point (- current-x 1) current-y)      ; left
        (make-point (+ current-x 1) current-y)      ; right
        (make-point (- current-x 1) (- current-y 1)) ; lower-left
        (make-point current-x (- current-y 1))      ; lower
        (make-point (+ current-x 1) (- current-y 1)))) ; lower-right
 
(define (print-coordinates cell)
  (newline)
  (display "Coordinates: ")
  (display "(")
  (display (car cell))
  (display ", ")
  (display (cdr cell))
  (display ")")
)
 
(define (print-board)
  (display live-cells)
  (newline))
 
(define (run-one-step)
  (let ((new-live-cells (create-new-live-cells)))
  (update-live-cells new-live-cells)
  (print-board)))
 
(define (run-n-iterations n)
  (let loop ((i 1)) ; start with 1
    (when (<= i n) ; until i > n
      (newline)
      (display "Iteration ")
      (display i)
      (display "----------------------")
      (newline)
      (run-one-step)
      (loop (+ i 1))))) ; increment i by 1
;-------------------
; testing
 
; the square cells should stay the same
;(define initial-live-cells
;  (list (make-cell (make-point 1 1))
;        (make-cell (make-point 1 2))
;        (make-cell (make-point 2 1))
;        (make-cell (make-point 2 2))))
 
; walker
(define initial-live-cells
  (list (make-cell (make-point 1 3))
        (make-cell (make-point 1 2))
        (make-cell (make-point 1 1))
        (make-cell (make-point 2 1))
        (make-cell (make-point 3 2))))
 
(update-live-cells initial-live-cells)
 
(display "Initial Cell Coordinates")
(print-board) ; initial cells
 
;(run-one-step)
(run-n-iterations 10000)
