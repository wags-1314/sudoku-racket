#lang racket

(require "utils.rkt")

(provide replace-2d check-board? add-hints-to-board reduce-board 4x4)

(define board '((#f #f #f  3 #f #f)
                (#f #f  1 #f #f #f)
                (#f  4 #f  2 #f  3)
                (#f  2 #f  6 #f  5)
                ( 2 #f #f #f #f #f)
                (#f  5  6 #f #f #f)))

(define 4x4 '((0 4 0 2)
              (0 0 1 0)
              (0 3 0 1)
              (0 1 4 3)))

(define (replace-2d i j el lst)
  (define new-row (replace-at el j (list-ref lst i)))
  (replace-at new-row i lst))

(define (check-rows? M*N board)
  (/>> board
       (map (curry uniq-range? M*N))
       (andmap identity)))

(define (check-columns? M*N board)
  (/>> board
       transpose
       (check-rows? M*N)))

(define (rows->blocks M N board)
  (/>> board
       (map (curry break-into M))
       (break-into N)
       (map transpose)
       append*
       (map append*)))

(define (check-blocks? M N board)
  (/>> board
       (rows->blocks M N)
       (check-rows? (* M N))))

(define (check-board? M N board)
  (and (check-rows? (* M N) board)
       (check-columns? (* M N) board)
       (check-blocks? M N board)))

;- Solving with Hints ------------------------------------------------

(define (add-hints M*N lst)
  (define hints (make-range M*N))
  (define (recurse lst)
    (match lst
      ['() '()]
      [(list head tail ...) (cons (if (zero? head) hints head) (recurse tail))]))
  (recurse lst))

(define (add-hints-to-board M*N board)
  (map (curry add-hints M*N) board))

(define (reduce-hints row)
  (define nums (filter number? row))
  (map (lambda (el)
         (if (list? el)
             (remove* nums el)
             el))
       row))

(define (fix-nums row)
  (match row
    ['() '()]
    [(list head tail ...) (cons (if (and (list? head)
                                         (= 1 (length head)))
                                    (car head)
                                    head)
                                (fix-nums tail))]))

(define (reduce-board-hints M N board)
  (/>> board
       ;; reduce for rows
       (map reduce-hints)
       ;; reduce for columns
       transpose (map reduce-hints) transpose
       ;; reduce for groups
       (rows->blocks M N) (map reduce-hints) (rows->blocks M N)
       ;; change single hints (like `(4)`) into numbers
       fix-nums))

(define (plateau func x)
  (define (recurse x)
    (define new_x (func x))
    (if (equal? x new_x)
        x
        (recurse new_x)))
  (recurse x))

(define (reduce-board M N board)
  (plateau (curry reduce-board-hints M N) board))
