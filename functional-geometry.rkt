#lang racket


(require racket/draw)
(require racket/gui)


;; vector constructor/selectors
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

;; vector operations
(define (transpose v)
  (make-vect (ycor-vect v) (xcor-vect v)))
(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
             (+ (ycor-vect v) (ycor-vect w))))
(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w))
             (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect v s)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; vector blocks
(define i-hat (make-vect 1 0))
(define j-hat (transpose i-hat))

(define origin-vect (make-vect 0 0))
(define diag-vect (make-vect 1 1))
(define split-vect (make-vect 0.5 0))

;; segment constructor/selectors
(define (make-segment x y) (cons x y))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;; frame constructor/selectors
(define (make-frame orig e1 e2 dc)
  (list orig e1 e2 dc))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))
(define (dc frame) (cadddr frame)) ; bitmap Display Context
