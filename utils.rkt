#lang racket

(provide uniq-range? transpose break-into /> />> replace-at make-range)

(define (uniq-range? len lst)
  (for/and ([i (in-range 1 (+ len 1))])
    (member i lst)))

(module+ test
  (require rackunit)
  (check-not-false (uniq-range? 5 '(1 2 3 4 5)))
  (check-not-false (uniq-range? 5 '(5 4 3 2 1)))
  (check-not-false (uniq-range? 5 '(5 1 4 3 2)))
  (check-false (uniq-range? 5 '(1 1 1 1 1))))

(define (transpose lst-of-lsts)
  (apply map list lst-of-lsts))

(module+ test
  (define matrix '((1 2) (3 4)))
  (check-equal? '((1 3) (2 4)) (transpose matrix)))

(define (break-into n lst)
  (cond
    [(null? lst) lst]
    [(>= (length lst) n) (cons (take lst n)
                               (break-into n (drop lst n)))]
    [else (list lst)]))

(module+ test
  (check-equal? '((1 2) (3 4) (5 6)) (break-into 2 '(1 2 3 4 5 6)))
  (check-equal? '((1 2 3 4 5)) (break-into 6 '(1 2 3 4 5)))
  (check-equal? '() (break-into 2 '())))

(define (replace-at el i lst)
  (define (recurse idx lst)
    (match lst
      ['() '()]
      [(list head tail ...)
       (if (= i idx)
           (cons el tail)
           (cons head
                 (recurse (+ idx 1) tail)))]))
  (recurse 0 lst))

(define (make-range n)
  (if (= n 0)
      '()
      (cons n (make-range (- n 1)))))

;- Piping Macro ----------------------------------------------------
; My brain does not comprehend nesting functions

(define-syntax />
  (syntax-rules ()
    [(/> val) val]
    [(/> val (fun args ...) funs ...) (/> (fun val args ...)
                                          funs ...)]
    [(/> val fun funs ...) (/> (fun val) funs ...)]))

(define-syntax />>
  (syntax-rules ()
    [(/>> val) val]
    [(/>> val (fun args ...) funs ...) (/>> (fun args ... val)
                                            funs ...)]
    [(/>> val fun funs ...) (/>> (fun val) funs ...)]))

