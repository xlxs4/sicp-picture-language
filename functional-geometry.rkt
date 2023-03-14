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
