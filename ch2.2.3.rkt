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

;; Exercise 2.20 차례열 연산 인터페이스를 적용해보면..
(define (same-parity first-n . lists)
  (let ((condition? (if (even? first-n)
                    even?
                    odd?)))
    (cons first-n (accumulate cons
                              nil
                              (filter condition?
                                      lists)))))

;; 연습문제 ========================================================================

;; ex 2.33
(define (map-1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;; ex 2.34
;; accmulate로 a_nX^n + a_(n-1)X^(n-1) + ... + a1X + a0 계산하는 프로시저 짜기
;; 위 식은 (...(((a_n*x + a_(n-1))*x + a_(n-2))*x + ... + a_1) + a_0와 같다. 이를 Horner's rule이라 함.
;; http://en.wikipedia.org/wiki/Horner%27s_rule
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))
;; (horner-eval 2 (list 1 3 0 5 0 1)) ; > 79
;; coefficient-sequence의 순서가 반대다.
;; x^5 + 5x^3 + 3x + 1 일때 coeff-seq는 (1 3 0 5 0 1)로 들어간다.

;; ex 2.35
;; accumulate로 count-leaves 짜기
(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (x) (length (enumerate-tree x))) tree)))
;; accumulate와 map이 있어서 위와 같이 짰는데 그다지 효율적이지 않은듯. 차라리 아래와 같이 짜는게??
(define (count-leaves-1 tree)
  (length (enumerate-tree tree)))
;; 아님 더 좋은 방법이 있을까?
;; 이렇게도? 결국 처음것이랑 동일한듯.
(define (count-leaves-2 tree)
  (accumulate (lambda (seq sum)
                (+ sum (length seq)))
              0
              (map (lambda (x) (enumerate-tree x)) tree)))

;; ex 2.36
;; (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) => (22 26 30)
;; 세번째 인자가 list를 가진 list이고 각 list의 길이는 동일하다.
;; 값 list의 원소 순서대로 연산을 accumulate한 결과 list를 반환한다.
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; ex 2.37
;; ex2.37.rkt 파일 참조

;; ex 2.38
(define fold-right accumulate)
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(fold-right / 1 (list 1 2 3)) ; > (/ 1 (/ 2 (/ 3 1))) > 3/2
(fold-left / 1 (list 1 2 3))  ; > (/ (/ (/ 1 1) 2) 3) > 1/6
(fold-right list (list) (list 1 2 3)) ; > (1 (2 (3 ())))
(fold-left list (list) (list 1 2 3))  ; > (((() 1) 2) 3)
;; fold-right와 fold-left의 연산 결과가 동일하려면 교환법칙이 성립하는 연산자여야 한다. 즉 A op. B = B op. A 이어야 한다.
;; (= (fold-right * 1 (list 1 2 3)) (fold-left * 1 (list 1 2 3))) > #t
;; (= (fold-right + 1 (list 1 2 3)) (fold-left + 1 (list 1 2 3))) > #t

;; ex 2.39
;; fold-left와 fold-right로 reverse 프로시저 정의하기
(define (reverse-fr sequence)
  (fold-right (lambda (x y) (append y (list x))) (list) sequence))

(define (reverse-fl sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))

;(accumulate append
;            (list)
;            (map (lambda (j)
;                   (map (lambda (i) (list i j)) 
;                        (enumerate-interval 1 (- j 1))))
;                 (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

;; flatmap을 이용하여 위 프로시저 쓰면
;(flatmap (lambda (j)
;           (map (lambda (i) (list i j)) 
;                (enumerate-interval 1 (- j 1))))
;         (enumerate-interval 1 n))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder a b) 0))
  (define (square x)
    (* x x))
  (define (find-divisor value test-div)
    (cond ((divides? value test-div) test-div)
          ((> (square test-div) value) value)
          (else (find-divisor value (+ test-div 1)))))
  (find-divisor n 2))

;; prime-sum? list의 합이 소수인지?
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; make-pair-sum pair와 pair 요소의 합으로 구성된 list 만들기
(define (make-pair-sum pair)
  (let ((f (car pair))
        (n (cadr pair)))
    (list f n (+ f n))))

;; prime-sum-pairs? 합이 소수인거
(define (prime-sum-pairs? n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (j)
                          (map (lambda (i) (list i j)) 
                               (enumerate-interval 1 (- j 1))))
                        (enumerate-interval 1 n)))))

(define (remove seq item)
  (filter (lambda (x) (not (= item x))) seq))

;; 위에서 사용한거 가지고 순열 만들기
(define (permutation s)
  (if (null? s)
      (list (list))
      (flatmap (lambda (m)
                 (map (lambda (n) (cons m n))
                      (permutation (remove s m)))) s)))
;; 음.. 이해한건가??

;; ex 2.40
;; unique-pairs 프로시저 정의하기. 1 <= j < i <= n 을 만족하는 (i, j) 쌍의 차례열 뽑는 프로시저
(define (unique-pairs n)
  (flatmap (lambda (j)
             (map (lambda (i) (list j i))
                  (enumerate-interval 1 (- j 1))))
           (enumerate-interval 1 n)))
;; unique-pairs 프로시저를 이용하여 prime-sum-pairs 정의 줄이기
(define (prime-sum-pairs-2? n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;; 위의 prime-sum-pairs 프로시저를 2개의 프로시저로 구분한거 밖에 없는듯.

;; ex 2.41
;; 원문: Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than or equal to a given integer n that sum to a given integer s.
;; 입력이 n, s이고 결과는 합이 s가 되는 n 이하의 모든 i,j,k triples를 순서대로 뽑아라.
;; 순서대로가 무슨 의미지?
;; s=10, n=10 > (1 2 7) (1 3 6) (1 4 5) (2 3 5)
;; s=10, n=5  > (1 4 5) (2 3 5)
(define (triple-sum s n)
  (filter (lambda (seq) (= (apply + seq) s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list k j i)) (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
