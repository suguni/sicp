#lang racket

;; element-of-set?, adjoin-set, union-set, intersection-set 프로시저가 있을때 아래가 성립한다.
;; (element-of-set? x (adjoin-set x S))                 ;; 항상 참
;; (= (element-of-set? x (union-set S T)) 
;;    (or (element-of-set? x S) (element-of-set? x T))) ;; 항상 참
;; (element-of-set? x '())                              ;; 항상 거짓

;; unordered list
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x (car set1)))
        (if (element-of-set? x set2)
            (adjoin-set x (intersection-set (cdr set1) set2))
            (intersection-set (cdr set1) set2)))))

;; ex 2.59
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (let ((x (car set1))
            (sub-set (cdr set1)))
        (if (element-of-set? x set2)
            (union-set sub-set set2)
            (adjoin-set x (union-set sub-set set2))))))

;; ex 2.60 - 중복이 허용하게 만들 경우
;; 동일
(define (element-of-set-d? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
;; 변경
(define (adjoin-set-d x set)
  (cons x set))
;; 동일
(define (intersection-set-d set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x (car set1))
            (sub-set (cdr set1)))
        (if (element-of-set-d? x set2)
            (adjoin-set-d x (intersection-set-d sub-set set2))
            (intersection-set-d sub-set set2)))))
;; 변경
(define (union-set-d set1 set2)
  (if (null? set1)
      set2
      (adjoin-set-d (car set1) (union-set-d (cdr set1) set2))))
;; 효율 - 중복 허용이 효율이 높다. 
;;        adjoin-set의 계산은 element-of-set? 이 호출되지 않으므로 OMEGA(1)
;;        intersection-set 에서는 element-of-set?을 호출하므로 OMEGA(n)
;;        union-set 에서는 호출되지 않으므로 OMEGA(1) ???
;; 더 잘 맞아떨어지는 문제? - 공간 효율성이 문제가 되지 않는 경우. 실 사례???

;; ordered-list 인 경우
(define (element-of-set-ol? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set-ol? x (cdr set)))))

(define (intersection-of-set-ol? set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2))
            (sub-set1 (cdr set1))
            (sub-set2 (cdr set2)))
        (cond ((equal? x1 x2) (adjoin-set x1 (intersection-of-set-ol? sub-set1 sub-set2)))
              ((> x1 x2) (intersection-of-set-ol? set1 sub-set2))
              ((< x1 x2) (intersection-of-set-ol? sub-set1 set2))))))

;; ex 2.61
(define (adjoin-set-ol x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin-set-ol x (cdr set))))))
;; 차례 매김 표현 방식의 좋은 점이 어떻게 도움이 되는가?
;;   모든 원소를 확인할 필요가 없다. element-of-set을 호출할 필요가 없다.

;; ex 2.62
(define (union-set-ol set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2))
               (sub-set1 (cdr set1))
               (sub-set2 (cdr set2)))
           (cond ((equal? x1 x2) (adjoin-set-ol x1 (union-set-ol sub-set1 sub-set2)))
                 ((> x1 x2)      (adjoin-set-ol x2 (union-set-ol set1 sub-set2)))
                 ((< x1 x2)      (adjoin-set-ol x1 (union-set-ol sub-set1 set2))))))))
;; 이게 OMEGA(n)인가?
