#lang racket


(require racket/draw)
(require racket/gui)


;; vector constructor/selectors
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
