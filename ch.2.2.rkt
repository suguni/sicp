#lang racket

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;(define (length items)
;  (if (null? items)
;      0
;      (+ 1 (length (cdr items)))))

(define (length items)
  (define (iter l n)
    (if (null? l)
        n
        (iter (cdr l) (+ n 1))))
  (iter items 0))

;(define (append items1 items2)
;  (if (null? items2)
;      items1
;      (append (cons (car items2) items1) (cdr items2))))

(define (append items1 items2)
  (if (null? items1)
      items2
      (cons (car items1) (append (cdr items1) items2))))

(define i1 (list 'a 'b 'c))
(define i2 (list 'd 'e 'f))

;; ex 2.17
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;; ex 2.18
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))
