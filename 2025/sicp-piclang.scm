(define-module (sicp-piclang)
  #:export (segments->painter
            segments->shape
            transform-painter
            make-vect-answer
            xcor-vect-answer
            ycor-vect-answer
            add-vect-answer
            sub-vect-answer
            scale-vect-answer
            make-segment-answer
            start-segment-answer
            end-segment-answer
            make-frame
            origin-frame-answer
            edge1-frame-answer
            edge2-frame-answer
            paint-lines
            wave-answer
            flip-vert
            flip-horiz-answer
            below-answer
            beside
            up-split-answer
            right-split
            corner-split
            square-limit))

(use-modules (pict))

(define (make-vect-answer x y)
  ;; a vector is a list of two coordinates. An alternate representation would be a cons pair.
  (list x y))

(define xcor-vect-answer car)

(define ycor-vect-answer cadr)

(define (add-vect-answer v w)
  (list (+ (xcor-vect-answer v) (xcor-vect-answer w))
        (+ (ycor-vect-answer v) (ycor-vect-answer w))))

(define (sub-vect-answer v w)
  (list (- (xcor-vect-answer v) (xcor-vect-answer w))
        (- (ycor-vect-answer v) (ycor-vect-answer w))))

(define (scale-vect-answer s v)
  (list (* s (xcor-vect-answer v))
        (* s (ycor-vect-answer v))))

(define (make-frame origin edge1 edge2)
  ;; a frame is a list of three vectors
  (list origin edge1 edge2))

(define origin-frame-answer car)

(define edge1-frame-answer cadr)

(define edge2-frame-answer caddr)

(define (make-segment-answer start end)
  ;; a segment is a list of two vectors
  (list start end))

(define start-segment-answer car)

(define end-segment-answer cadr)

(define segments->shape list)

(define superimpose-shapes append)

(define (frame-coord-map frame)
  ;; Returns a function for adjusting a frame by a vector 
  (lambda (v)
    (add-vect-answer
     (origin-frame-answer frame)
     (add-vect-answer 
      (scale-vect-answer (xcor-vect-answer v)
                         (edge1-frame-answer frame))
      (scale-vect-answer (ycor-vect-answer v)
                         (edge2-frame-answer frame))))))

(define (draw-line start end)
  ;; take two vectors, returns a line SVG object for pict
  (line (xcor-vect-answer start)
        (ycor-vect-answer start)
        (xcor-vect-answer end)
        (ycor-vect-answer end)))

(define (segments->painter segment-list)
  ;; takes a list of segments, returns a "painter" lambda, which applies a frame
  ;; to those segments, and then maps over the result with draw-line to make a
  ;; list of SVG line objects which pict can combine.
  (lambda (frame)
    (map
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment-answer segment))
        ((frame-coord-map frame)
         (end-segment-answer segment))))
     segment-list)))


;; NOTE: in the text, draw-line is a function which triggers an action in
;; some graphics driver, and returns nothing. Because of this, (map) was
;; originally (for-each). Thus the final result would have been thrown away.

(define (transform-painter painter origin corner1 corner2)
  ;; produces a new painter by applying the supplied painter to an affine transformation specified by the
  ;; three argument vectors. Is it possible to merge frame-coord-map and transform-painter somehow?
  (lambda (frame)
    (let* ((m (frame-coord-map frame))
           (new-origin (m origin)))
      (painter (make-frame new-origin
                           (sub-vect-answer (m corner1) new-origin)
                           (sub-vect-answer (m corner2) new-origin))))))

(define (paint-lines painter)
  ;; use pict to compile an SVG with the elements described by painter
  (let ((Frame (make-frame (make-vect-answer 0 0)
                           (make-vect-answer 500 0)
                           (make-vect-answer 0 500))))
    (apply lt-superimpose
           (painter Frame))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect-answer 0.0 1.0)  ;; new origin
                     (make-vect-answer 1.0 1.0)  ;; new end of edge1
                     (make-vect-answer 0.0 0.0)));; new end of edge2

(define (flip-horiz-answer painter)
  (transform-painter painter
                     (make-vect-answer 1.0 0.0)
                     (make-vect-answer 0.0 0.0)
                     (make-vect-answer 1.0 1.0)))

(define (beside painter1 painter2)
  (let* ((split-point (make-vect-answer 0.5 0.0))
          (paint-left  (transform-painter
                        painter1
                        (make-vect-answer 0.0 0.0)
                        split-point
                        (make-vect-answer 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect-answer 1.0 0.0)
                        (make-vect-answer 0.5 1.0))))
    (lambda (frame)
      (superimpose-shapes ;; <- the key change
       (paint-left frame)
       (paint-right frame)))))

(define (below-answer painter1 painter2)
  (let* ((split-point (make-vect-answer 0.0 0.5))
         (paint-below-answer (transform-painter painter1
                                         split-point
                                         (make-vect-answer 1.0 0.5)
                                         (make-vect-answer 0.0 1.0)))
         (paint-above (transform-painter painter2
                                         (make-vect-answer 0.0 0.0)
                                         (make-vect-answer 1.0 0.0)
                                         split-point)))
    (lambda (frame)
      (superimpose-shapes
       (paint-above frame)
       (paint-below-answer frame)))))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below-answer smaller smaller)))))

(define (up-split-answer painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split-answer painter (- n 1))))
        (below-answer painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let* ((up (up-split-answer painter (- n 1)))
             (right (right-split painter (- n 1)))
             (top-left (beside up up))
             (bottom-right (below-answer right right))
             (corner (corner-split painter (- n 1))))
        (beside (below-answer painter top-left)
                (below-answer bottom-right corner)))))

(define (square-limit painter n)
  (let* ((quarter (corner-split painter n))
         (half (beside (flip-horiz-answer quarter) quarter)))
    (below-answer (flip-vert half) half)))

;; wave-answer starts here

(define legs
  (segments->shape (make-segment-answer (make-vect-answer 0.35 0.65)
                                        (make-vect-answer 0.25 1))
                   (make-segment-answer (make-vect-answer 0.65 0.65)
                                        (make-vect-answer 0.75 1))
                   (make-segment-answer (make-vect-answer 0.3 1)
                                        (make-vect-answer 0.5 0.7))
                   (make-segment-answer (make-vect-answer 0.7 1)
                                        (make-vect-answer 0.5 0.7))))

(define left-arm
  (segments->shape (make-segment-answer (make-vect-answer 0.45 0.2)
                                        (make-vect-answer 0.35 0.15))
                   (make-segment-answer (make-vect-answer 0.35 0.15)
                                        (make-vect-answer 0.15 0.3))
                   (make-segment-answer (make-vect-answer 0.15 0.3)
                                        (make-vect-answer 0 0.15))
                   (make-segment-answer (make-vect-answer 0.4 0.3)
                                        (make-vect-answer 0.15 0.45))
                   (make-segment-answer (make-vect-answer 0.15 0.45)
                                        (make-vect-answer 0 0.2))))

(define right-arm
  (segments->shape (make-segment-answer (make-vect-answer 0.55 0.2)
                                        (make-vect-answer 0.65 0.15))
                   (make-segment-answer (make-vect-answer 0.65 0.15)
                                        (make-vect-answer 1 0.55))
                   (make-segment-answer (make-vect-answer 0.6 0.3)
                                        (make-vect-answer 1 0.65))))

(define torso
  (segments->shape (make-segment-answer (make-vect-answer 0.4 0.3)
                                        (make-vect-answer 0.35 0.65))
                   (make-segment-answer (make-vect-answer 0.6 0.3)
                                        (make-vect-answer 0.65 0.65))))
(define head
  (segments->shape (make-segment-answer (make-vect-answer 0.45 0)
                                        (make-vect-answer 0.4 0.1))
                   (make-segment-answer (make-vect-answer 0.55 0)
                                        (make-vect-answer 0.6 0.1))
                   (make-segment-answer (make-vect-answer 0.4 0.1)
                                        (make-vect-answer 0.45 0.2))
                   (make-segment-answer (make-vect-answer 0.6 0.1)
                                        (make-vect-answer 0.55 0.2))))

(define wave-answer
  (segments->painter (superimpose-shapes head torso legs right-arm left-arm)))
