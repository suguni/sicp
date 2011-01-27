#lang racket

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
(paint wave)
