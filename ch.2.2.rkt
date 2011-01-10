#lang racket

;; common procedure
(define (square x)
  (* x x))

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
;(define (last-pair items)
;  (if (null? (cdr items))
;      items
;      (last-pair (cdr items))))
;; 위 코드는 (last-pair (list)) 이면 에러
(define (last-pair items)
  (define (iter l)
    (let ((rest (cdr l)))
      (if (null? rest)
          l
          (iter rest))))
  (if (null? items)
      items
      (iter items)))

;; ex 2.18
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))
;; 이거 너무 속도가 느린거 같은데.. n^2
;; append를 사용하지 않는 버전 ???
(define (reverse-2 items)
  (define (iter ins outs)
    (if (null? ins)
        outs
        (iter (cdr ins) (cons (car ins) outs))))
  (iter items (list)))
;; recursive 하게 짜는건 포기. append를 사용하지 않고는 힘듬.

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


;; p.136
(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items)) (map proc (cdr items)))))

;; ex 2.21
;; (square-list (list 1 2 3 4)) => (1 4 9 16) 이 되게 두 가지 방법으로 프로시저 짜기
(define (square-list-1 items)
  (if (null? items)
      (list)
      (cons (* (car items) (car items)) ;; (square (car items)) 로 해도 됨.
            (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

;; ex 2.22
(define (square-list-iter-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items (list)))
;; 위 코드는 순서가 반대로 나온다. 왜?
;; => (cons (square (car things)) answer) 때문. 이렇게 하면 뒤에서 부터 채워진다.
;; (square-list-iter-1 (list 1 2 3 4)) 이면, answer가 만들어지는 방식은
;; (cons 1 ())
;; (cons 4 (1))
;; (cons 9 (4 1))
;; (cons 16 (9 4 1))
;; (16 9 4 1)
;; 이 된다.

(define (square-list-iter-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items (list)))
;; 이렇게 하면 list 요소가 list가 된다.
;; (square-list-iter-2 (list 1 2 3 4)) => ((((() . 1) . 4) . 9) . 16)
;; cons의 첫번째 인자가 list 이기 때문.

;; ex 2.23
;; for-each 프로시저 짜기.
(define (for-each proc items)
  (if (null? items)
      (list)
      (let ((dummy (proc (car items))))
        (for-each proc (cdr items)))))
;; proc 프로시저를 실행할 방법이 필요한데, 위에서는 let을 이용함.
;; map에서 했던거 처럼 cons를 사용하는데 대신 의미 없음.
(define (for-each-1 proc items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (for-each-1 proc (cdr items)))))
;; http://community.schemewiki.org/?sicp-ex-2.23에서는 if 대신 cond를 사용함.
;; if는 true와 false에 하나의 form만 올 수 있는데 반해 cond는 여러개의 form이 올 수 있음.
(define (for-each-2 proc items)
  (cond ((not (null? items))
         (proc (car items))
         (for-each-2 proc (cdr items)))))
;; Racket에서는 begin special form을 사용할 수 있다. side-effect가 필요한 경우 사용.
(define (for-each-4 proc items)
  (if (null? items)
      (list)
      (begin (proc (car items))
             (for-each-4 proc (cdr items)))))
