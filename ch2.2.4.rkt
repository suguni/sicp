#lang racket

;; references http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; original source - http://d.hatena.ne.jp/kazu-yamamoto/20100524/1274666251
;; special thanks to kazu-yamamoto
(define wave (segments->painter
              (list
                   (make-segment (make-vect 0.00 0.65) (make-vect 0.15 0.40))
                   (make-segment (make-vect 0.15 0.40) (make-vect 0.30 0.60))
                   (make-segment (make-vect 0.30 0.60) (make-vect 0.35 0.55))
                   (make-segment (make-vect 0.35 0.55) (make-vect 0.25 0.00))
                   (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))
                   (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))
                   (make-segment (make-vect 0.75 0.00) (make-vect 0.60 0.50))
                   (make-segment (make-vect 0.60 0.50) (make-vect 1.00 0.15))
                   (make-segment (make-vect 1.00 0.35) (make-vect 0.75 0.65))
                   (make-segment (make-vect 0.75 0.65) (make-vect 0.60 0.65))
                   (make-segment (make-vect 0.60 0.65) (make-vect 0.65 0.85))
                   (make-segment (make-vect 0.65 0.85) (make-vect 0.60 1.00))
                   (make-segment (make-vect 0.40 1.00) (make-vect 0.35 0.85))
                   (make-segment (make-vect 0.35 0.85) (make-vect 0.40 0.65))
                   (make-segment (make-vect 0.40 0.65) (make-vect 0.30 0.65))
                   (make-segment (make-vect 0.30 0.65) (make-vect 0.15 0.60))
                   (make-segment (make-vect 0.15 0.60) (make-vect 0.00 0.85)))))
;; (paint wave)

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

; (paint-hires wave2)
; (paint-hires wave4)

(define (flipped-pairs painter)
  (let ((painter2 (beside wave (flip-vert wave))))
    (below painter2 painter2)))
; (paint (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
; (paint-hires (right-split wave 4))

;; ex 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
;(paint-hires (up-split wave 4))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;(paint-hires (corner-split wave 4))

(define (square-limit painter n)
  (let ((c (corner-split painter n)))
    (let ((half (beside (flip-horiz c) c)))
      (below (flip-vert half) half))))
;(paint-hires (square-limit wave 4))

;; 차수 높은 연산
(define (square-of-fours tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity o) o)

(define (flipped-pairs-2 painter)
  (let ((op (square-of-fours identity flip-vert identity flip-vert)))
    (op painter)))

(define (square-limit-2 painter n)
  (let ((c4 (square-of-fours flip-horiz identity rotate180 flip-vert)))
    (c4 (corner-split painter n))))
;(paint-hires (square-limit-2 wave 4))

; ex 2.45 - split procedure
;; (define up-split (split beside below))
;; (define right-split (split below beside))
(define (split p1 p2)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (p2 painter (p1 smaller smaller)))))
  iter)

(define up-split-2 (split beside below))
;(paint (up-split-2 wave 4))
(define right-split-2 (split below beside))
;(paint (right-split-2 wave 4))


;; 그림틀

;; ex 2.46
(define (make-vector x y)(cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vector (+ (xcor-vect v1) (xcor-vect v2))
               (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vector (- (xcor-vect v1) (xcor-vect v2))
               (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vector (* (xcor-vect v) s)
               (* (ycor-vect v) s)))

;; test
;(= (xcor-vect (make-vector 1 2)) 1)
;(= (ycor-vect (make-vector 1 2)) 2)
;(equal? (add-vect (make-vector 1 5) (make-vector 3 2)) (make-vector 4 7))
;(equal? (sub-vect (make-vector 1 5) (make-vector 3 2)) (make-vector -2 3))
;(equal? (scale-vect (make-vector 1 5) 3) (make-vector 3 15))

;; ex 2.47
(define (make-frame-2-1 origin edge1 edge2) (list origin edge1 edge2))
(define (origin-frame-2-1 frame) (car frame))
(define (edge1-frame-2-1 frame) (car (cdr frame)))
(define (edge2-frame-2-1 frame) (car (cdr (cdr frame))))
;; test
(let ((o (make-vector 3 3))
      (e1 (make-vector 10 2))
      (e2 (make-vector 2 10)))
  (let ((f (make-frame-2-1 o e1 e2)))
    (format "~a ~a ~a"
            (equal? (origin-frame-2-1 f) o)
            (equal? (edge1-frame-2-1 f) e1)
            (equal? (edge2-frame-2-1 f) e2))))

(define (make-frame-2-2 origin edge1 edge2) (cons origin (cons edge1 edge2)))
(define (origin-frame-2-2 frame) (car frame))
(define (edge1-frame-2-2 frame) (car (cdr frame)))
(define (edge2-frame-2-2 frame) (cdr (cdr frame)))
;; test
(let ((o (make-vector 3 3))
      (e1 (make-vector 10 2))
      (e2 (make-vector 2 10)))
  (let ((f (make-frame-2-2 o e1 e2)))
    (format "~a ~a ~a"
            (equal? (origin-frame-2-2 f) o)
            (equal? (edge1-frame-2-2 f) e1)
            (equal? (edge2-frame-2-2 f) e2))))

(define make-frame-2 make-frame-2-1)
(define origin-frame-2 origin-frame-2-1)
(define edge1-frame-2  edge1-frame-2-1)
(define edge2-frame-2  edge2-frame-2-1)

(define (frame-coord-map-2 frame)
  (lambda (v)
    (add-vect
     (origin-frame-2 frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame-2 frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame-2 frame))))))

;; 페인터
