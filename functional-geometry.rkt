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

;; transform an image to fit the frame
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

;; painter generator
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start ((frame-coord-map frame) (start-segment segment)))
             (end ((frame-coord-map frame) (end-segment segment))))
         (send (dc frame) draw-line
               (xcor-vect start)
               (ycor-vect start)
               (xcor-vect end)
               (ycor-vect end))))
     segment-list)))

;; painters
(define (frame-outline frame)
  ((segments->painter (list (make-segment origin-vect i-hat)
                            (make-segment i-hat diag-vect)
                            (make-segment diag-vect j-hat)
                            (make-segment j-hat origin-vect))) frame))

(define (frame-cross frame)
  ((segments->painter (list (make-segment origin-vect diag-vect)
                            (make-segment i-hat j-hat))) frame))

(define (frame-diamond frame)
  ((segments->painter (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                            (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                            (make-segment (make-vect 1 0.5) split-vect)
                            (make-segment split-vect (make-vect 0 0.5)))) frame))

;; transformer generator
(define (transform-painter painter orig corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-orig (m orig)))
        (painter (make-frame new-orig
                             (sub-vect (m corner1) new-orig)
                             (sub-vect (m corner2) new-orig)
                             (dc frame)))))))
