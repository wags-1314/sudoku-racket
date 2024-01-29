#lang racket/gui

;; Sudoku Solver
;; By Bhargav Kulkarni

(require "sudoku.rkt" "utils.rkt")

(define M 2)
(define N 2)
(define M*N (* M N))
(define cell-space 35)

(define frame
  (new frame%
       [label "Sudoku Solver"]))
(define canvas
  (new canvas% [parent frame]
       [min-width (* (+ M*N 2) cell-space)]
       [min-height (* (+ M*N 2) cell-space)]
       [paint-callback
        (lambda (canvas dc)
          (dc-draw-board dc)
          (dc-draw-numbers dc game-board))]))

(define 6x6 '((6 0 2 0 0 0)
              (0 5 0 2 0 0)
              (0 0 6 0 0 0)
              (0 0 3 5 6 1)
              (3 0 0 1 0 2)
              (0 4 1 0 0 0)))

(define 4x4-empty '((0 0 0 0)
                    (0 0 0 0)
                    (0 0 0 0)
                    (0 0 0 0)))

(define 9x9 '((0 0 0 0 3 0 0 0 1)
              (0 0 6 0 8 2 0 3 0)
              (0 0 0 6 4 0 0 2 0)
              (9 0 0 0 0 0 0 0 6)
              (2 0 0 3 9 6 0 0 0)
              (5 0 0 0 0 4 3 9 0)
              (0 9 0 0 0 3 5 6 0)
              (7 0 0 2 0 1 4 8 0)
              (0 0 2 8 0 9 0 0 0)))

(define 6x6-empty '((0 0 0 0 0 0)
                    (0 0 0 0 0 0)
                    (0 0 0 0 0 0)
                    (0 0 0 0 0 0)
                    (0 0 0 0 0 0)
                    (0 0 0 0 0 0)))

(define 9x9-empty '((0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0)))

;; game-board: Listof(Listof(num | Listof(num)))
(define game-board (box 9x9))


(define dc (send canvas get-dc))

(define (*redraw-board*)
  (send dc clear)
  (send canvas refresh-now))

(define (dc-draw-board dc)
  (define start-x cell-space)
  (define start-y cell-space)
  (define no-of-lines (+ (* M N) 1))
  (send dc set-pen "black" 1 'solid)
  (for ([i (in-range no-of-lines)])
    (if (= 0 (remainder i N))
        (send dc set-pen "black" 3 'solid)
        (send dc set-pen "black" 1 'solid))
    (send dc draw-line
          start-x
          (+ start-y (* cell-space i))
          (+ start-x (* cell-space M N))
          (+ start-y (* cell-space i)))
    (if (= 0 (remainder i M))
        (send dc set-pen "black" 3 'solid)
        (send dc set-pen "black" 1 'solid))
    (send dc draw-line
          (+ start-x (* cell-space i))
          start-y
          (+ start-x (* cell-space i))
          (+ start-y (* cell-space M N)))))

(define (dc-draw-numbers dc board)
  (send dc set-font (make-font #:size 18))
  (define start-x (+ cell-space 15))
  (define start-y (+ cell-space 5))
  (for* ([i (in-range (* M N))]
         [j (in-range (* M N))])
    (define el
      (list-ref (list-ref (unbox board) j) i))
    (send dc draw-text
          (if (number? el)
              (number->string el)
              "")
          (+ start-x (* cell-space i))
          (+ start-y (* cell-space j)))))

(define (draw-board board)
  (sleep/yield 0.01)
  (set-box! game-board
            board)
  (*redraw-board*))


;- The Solver --------------------------------------------------------

(define 4x4 '((0 4 0 2)
              (0 0 1 0)
              (0 3 0 1)
              (0 1 4 3)))

(define (solve M N board draw)
  (define M*N (* M N))
  (define (recurse i j board)
    (define (recurse-next i j board)
      (if (= (+ j 1) M*N)
          (recurse (+ i 1) 0 board)
          (recurse i (+ j 1) board)))

    (if (= i M*N)
        (and (check-board? M N board)
             board)

        (let ([el (list-ref (list-ref board i) j)])
          (cond
            [(list? el)
             (ormap identity
                    (for/list ([ment (in-list (shuffle el))])
                      (define new-board
                        (reduce-board M N (replace-2d i j ment board)))
                      (when draw (draw-board new-board))
                      (recurse-next i j new-board)))]
            [else (recurse-next i j board)]))))
  (recurse 0 0 board))


(send frame show #t)

(define (sudoku board [draw true])
  (define new-board (add-hints-to-board M*N board))
  (define reduced-board (reduce-board M N new-board))
  (when draw (set-box! game-board reduced-board))
  (define solved-board (solve M N reduced-board draw))
  (when draw (set-box! game-board solved-board) (*redraw-board*))
  solved-board)

; Could generate forever but, the chance of getting the same number that is
; already in `result` diminshes for every iteration
(define (generate-distinct-numbers n m)
  (let loop ([count 0] [result '()])
    (if (= count n)
        (reverse result)
        (let ([num (random m)])
          (if (not (member num result))
              (loop (add1 count) (cons num result))
              (loop count result))))))

(define (replace-elements-at-indices indices lst)
  (match indices
    ['() lst]
    [(cons index rest-indices)
     (replace-elements-at-indices rest-indices (list-set lst index 0))]))

;; generate-new-board : #|empty spaces| -> new sudoku board
(define (generate-empty-board [diff 9])
  (define empty-board (make-list M*N (make-list M*N 0)))
  (define solved-board (sudoku empty-board false))
  (define indices (generate-distinct-numbers diff (* M*N M*N)))
  (/>>
     solved-board
     append*
     (replace-elements-at-indices indices)
     (break-into M*N)))

(sudoku (generate-empty-board))
