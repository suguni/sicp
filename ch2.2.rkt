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

;; ch 2.2.2 계층구조
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((pair? tree) (+ (count-leaves (car tree)) (count-leaves (cdr tree))))
        (else 1)))

;;(define x (cons (list 1 2) (list 3 4)))

;; ex 2.24
(list 1 (list 2 (list 3 4))) ;; => (1 (2 (3 4)))

;; ex 2.25
(car (cdr (car (cdr (cdr
                     (list 1 3 (list 5 7) 9)
                     )))))
(car (car
      (list (list 7))
      ))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
                                                        (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))
                                                        ))))))))))))
     

;; ex 2.26
;(define x (list 1 2 3))
;(define y (list 4 5 6))
;(append x y) ;; => (1 2 3 4 5 6)
;(cons x y)   ;; => ((1 2 3) 4 5 6)
;(list x y)   ;; => ((1 2 3) (4 5 6))

;; ex 2.27
;; deep-reverse 프로시저 만들기
(define (deep-reverse items)
  (define (iter ins outs)
    (if (null? ins)
        outs
        (let ((item (car ins)))
          (iter (cdr ins)
                (cons (if (pair? item)
                          (iter item (list))
                          item)
                      outs)))))
  (iter items (list)))

;(reverse (list (list 1 2) (list 3 4))) ;; => ((3 4) (1 2))
;(deep-reverse (list (list 1 2) (list 3 4))) ;; => ((4 3) (2 1))
;(deep-reverse (list (list 1 2 3) 4 (list 5 6 (list 7 8 9) 10 11))) ;; => ((11 10 (9 8 7) 6 5) 4 (3 2 1))

;; ex 2.28
;; (fringe (list (list 1 2) (list 3 4))) ; => (1 2 3 4)
(define (fringe tree)
  (define (iter ins result)
    (if (null? ins)
        result
        (if (not (pair? (car ins)))
            (iter (cdr ins) (cons (car ins) result))
            (iter (cdr ins) (append (iter (car ins) (list)) result)))))
  (reverse (iter tree (list))))
;; 이거 왠지 무식한 방법인듯

(define (fringe-2 tree)
  (if (null? tree)
      (list)
      (let ((first (car tree))
            (next (cdr tree)))
        (if (pair? first)
            (append (fringe-2 first) (fringe-2 next))
            (cons first (fringe-2 next))))))
;; 이것도 append를 사용했는데..

(define (fringe-3 tree)
  (define (iter tree result)
    (if (null? tree)
        result
        (if (pair? tree)
            (iter (car tree) (iter (cdr tree) result))
            (cons tree result))))
  (iter tree (list)))
;; solution 참조. http://community.schemewiki.org/?sicp-ex-2.28

;; ex 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;; a.
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;; b.
;(define (total-weight mobile)
;  (if (null? mobile)
;      0
;      (let ((left (left-branch mobile))
;            (right (right-branch mobile)))
;        (+ (if (pair? (branch-structure left)) (total-weight (branch-structure left)) (branch-structure left))
;           (if (pair? (branch-structure right)) (total-weight (branch-structure right)) (branch-structure right))))))
;; 뭔가 중복이 많고... ???

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (pair? struct)
        (total-weight struct)
        struct)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; mobile, branch는 정확한 형태로 구성되어 있다고 가정됨.
;; make-mobile의 두 인자는 항상 make-branch로 만들어진 것이고 null이 오지 않음, make-branch도 인자로 null이 아님

;; c.
(define (branch-torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (mobile-balanced? mobile)
  (define (branch-balanced? branch)
    (let ((structure (branch-structure branch)))
      (if (pair? structure)
          (mobile-balanced? structure)
          true)))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (branch-torque left)
            (branch-torque right))
         (branch-balanced? left)
         (branch-balanced? right))))

;; d.
;; make-mobile, make-branch를 다음처럼 바꾸면?
(define (make-mobile-2 left right)
  (cons left right))
(define (make-branch-2 length structure)
  (cons length structure))
;; 프로그램 수정은? => selector만 아래처럼 변경하면 된다.
(define (right-branch-2 mobile) (cdr mobile))
(define (branch-structure-2 branch) (cdr branch))

;; 이건 naming을 통일하기 위함일뿐.
(define left-branch-2 left-branch)
(define branch-length-2 branch-length)

