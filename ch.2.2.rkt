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
;; 이거 너무 속도가 느린거 같은데.. n^2

;; append를 사용하지 않는 버전 ???




;; ex 2.19 -- skip


;; ex 2.20

;; dotted-tail notation
;; (define (f x y . z) (apply + z))

;; (same-parity 1 4 5 6 7 8 9) -> (1 5 7 9)
;; (same-parity 2 4 5 6 7 8 9) -> (2 4 6 8)
(define (same-parity x . items)
  (define (iter i o pred?)
    (if (null? i)
        o
        (iter (cdr i)
              (if (pred? (car i))
                  (cons (car i) o)
                  o)
              pred?)))
  (iter items (list x)
        (if (even? x) even? odd?)))
;; 이거 순서가 이상하다.

(define (same-parity-2 x . items)
  (let ((pred? (if (even? x) even? odd?)))
    (define (iter i)
      (if (null? i)
          (list)
          (if (pred? (car i))
              (cons (car i) (iter (cdr i)))
              (iter (cdr i)))))
    (cons x (iter items))))
;; OK!

