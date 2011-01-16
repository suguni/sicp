#lang racket

;; ch 2.2.3 공통 인터페이스로써 차례열의 쓰임새

(define (square x)
  (* x x))

;; 나무 잎사귀들 중 홀수인것들의 제곱을 모두 더한 값을 반환
;(define (sum-odd-squares tree)
;  (if (null? tree)
;      0
;      (if (not (pair? tree))
;          (if (odd? tree)
;              (square tree)
;              0)
;          (+ (sum-odd-squares (car tree))
;             (sum-odd-squares (cdr tree))))))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

         
;; (sum-odd-squares (list 1 (list 2 (list 3 4)) 5)) => 35

;; 일단 가장 단순하게 짜기
(define (fib n)
  (cond ((= n 0) 0)
        ((<= n 2) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

;; n 이하의 피보나치 수열값들 중에서 짝수만 모아서 리스트로 반환
(define (even-fibs n)
  (define (iter k)
    (if (<= k n)
        (let ((f (fib k)))
          (if (even? f)
              (cons f (iter (+ k 1)))
              (iter (+ k 1))))
        (list)))
  (iter 0))

;; 위 프로시저들을 공통 인터페이스 (enumerate, filter, map, accumulate) 로 짜기

(define (filter pred? seq)
  (if (null? seq)
      (list)
      (if (pred? (car seq))
          (cons (car seq) (filter pred? (cdr seq)))
          (filter pred? (cdr seq)))))

(define (map proc seq)
  (if (null? seq)
      (list)
      (cons (proc (car seq)) (map proc (cdr seq)))))

(define (accumulate proc initial seq)
  (if (null? seq)
      initial
      (proc (car seq)
            (accumulate proc initial (cdr seq)))))

;; enumerate 는 case-by-case

(define (enumerate-interval low high)
  (if (<= low high)
      (cons low (enumerate-interval (+ low 1) high))
      (list)))

;(define (enumerate-tree tree)
;  (cond ((null? tree) (list))
;        ((not (pair? tree)) (list tree))
;        (else (append (enumerate-tree (car tree))
;                      (enumerate-tree (cdr tree))))))

;; ex 2.28 위 enumerate-tree는 fringe procedure와 동일한데 다르게 풀었다...???

(define (enumerate-tree tree)
  (define (iter tree result)
    (cond ((null? tree) result)
          ((not (pair? tree)) (cons tree result))
          (else (iter (car tree) (iter (cdr tree) result)))))
  (iter tree (list)))

;; 다시 짜는 sum-odd-squares, even-fibs - enumerate, filter, map, accumulate 이용

(define (sum-odd-squares-1 tree)
  (accumulate + 0
              (map square 
                   (filter odd?
                           (enumerate-tree tree)))))
;; (sum-odd-squares-1 (list 1 (list 2 (list 3 4) 5)))
;; enumerate  > (1 2 3 4 5)
;; filter     > (1 3 5)
;; map        > (1 9 25)
;; accumulate > 35

(define (even-fibs-1 x)
  (filter even?
          (map fib
               (enumerate-interval 0 x))))
;; (even-fibs 10)
;; enumerate > (0 1 2 3 4 5 6 7 8 9 10)
;; map       > (0 1 1 2 3 5 8 13 21 34 55)
;; filter    > (0 2 8 34)

(define (list-fib-square n)
  (accumulate cons (list)
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
;; accumulate 할 필요 없는데..

