#lang racket

;; ch 2.3.3

;; 차례 없는 리스트 표현
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define set1 (list 1 3 5 7 9))
(define set2 (list 2 4 5 6 8 9 10 12))

;; 차례 매긴 리스트 표현
(define (element-of-set?-o x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set?-o x (cdr set)))))

(define (intersection-set-o set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-o (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set-o (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-o set1 (cdr set2)))))))


;; ex 2.61
(define (adjoin-set-o x set)
    (cond ((null? set) '())
          ((< x (car set)) (cons x set))
          ((= x (car set)) set)
          (else (cons (car set) (adjoin-set-o x (cdr set))))))
; x가 추가될 위치조건 이후는 don't care~~
