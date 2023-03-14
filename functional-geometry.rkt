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

;; transformers
(define (flip-vert painter)
  (transform-painter painter j-hat diag-vect origin-vect))
(define (flip-horiz painter)
  (transform-painter painter i-hat origin-vect diag-vect))

(define (shrink-to-upper-right painter)
  (let ((edge1 (make-vect 1 0.5)))
    (transform-painter painter
                       (scale-vect diag-vect 0.5)
                       edge1
                       (transpose edge1))))

(define (squash-inwards painter)
  (let ((edge1 (make-vect 0.65 0.35)))
    (transform-painter painter
                       (scale-vect diag-vect 0.35)
                       edge1
                       (transpose edge1))))

(define (rotate90 painter)
  (transform-painter painter i-hat diag-vect origin-vect))
(define (rotate180 painter) (rotate90 (rotate90 painter)))
(define (rotate270 painter) (rotate90 (rotate180 painter)))
(define (rotate360 painter) painter)

(define (paint-left painter)
  (transform-painter painter origin-vect split-vect j-hat))
(define (paint-right painter)
  (transform-painter painter split-vect i-hat (make-vect 0.5 1)))
(define (paint-top painter)
  (transform-painter painter origin-vect i-hat split-vect))
(define (paint-bot painter)
  (transform-painter painter split-vect (make-vect 1 0.5) j-hat))

(define (next-to transform1 transform2)
  (lambda (frame)
    (transform1 frame)
    (transform2 frame)))

;; painter composition
(define (beside painter1 painter2)
  (next-to (paint-left painter1) (paint-right painter2)))
(define (below painter1 painter2)
  (next-to (paint-top painter1) (paint-bot painter2)))

(define (split compose-main compose-smaller)
  (lambda (painter n)
    (if (zero? n) painter
        (let ((smaller ((split compose-main compose-smaller) painter (- n 1))))
          (compose-main painter (compose-smaller smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

;; main
(define target1 (make-bitmap 100 100))
(define dc1 (new bitmap-dc% [bitmap target1]))
(define frame1
  (let ((edge1 (scale-vect split-vect 100)))
    (make-frame origin-vect edge1 (transpose edge1) dc1)))

(make-object image-snip% target1)
(frame-diamond frame1)
