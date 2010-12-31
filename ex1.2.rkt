#lang racket

;; 1.2 
;; 어떤 문제를 풀기 위해 어떤 식으로 프로그래밍 언어를 써야 하는지 모르는 상태다.
;; 어떤 프로시저를 정의해야 하는데 프로시저를 돌릴 때 어떤 결과가 나오는지 미리 그려낼 줄 아는 경험 부족한 상태.
;; 프로시저: 한 컴퓨터 프로세스가 어떻게 나아가는지 밝힌 것.
;; 프로시저가 만들어내는 프로세스의 몇가지 꼴(shape)의 보기를 들것이다.

;; 1.2.1
;; 되도는 프로세스 recursion
;;  - 연산을 바로 수행하지 못하고 뒤로 미루는(deferred operation) 모양
;; 반복하는 프로세스 iterative
;;  - 상태변수 존재. 계산의 크기가 늘어나지 않는다.
;; 되도는 프로세스와 되도는 프로시저
;;  - 프로시저는 되도는 모양이더라도(자신을 다시 호출) 프로세스는 그렇지 않을 수 있다. - linera iterative process
;; tail recursion : 꼬리에서 자기 자신을 다시 호출하면서 recursive process가 안되도록 하는 것.
;;  - http://en.wikipedia.org/wiki/Recursion_%28computer_science%29#Tail-recursive_functions

;; ex 1.9
(define (inc a)
  (+ a 1))

(define (dec a)
  (- a 1))

(define (plus-1 a b)
  (if (= a 0)
      b
      (inc (plus-1 (dec a) b))))

;; (plus-1 2 3)
;; (inc (plus-1 1 3))
;; (inc (inc (plus-1 0 3)))
;; (inc (inc 3))
;; (inc 4)
;; 5

(define (plus-2 a b)
  (if (= a 0)
      b
      (plus-2 (dec a) (inc b))))
;; (plus-2 2 3)
;; (plus-2 1 4)
;; (plus-2 0 5)
;; 5


;; ex 1.10
;; 애커만 함수 http://en.wikipedia.org/wiki/Ackermann_function
;; primitive recursive function은 아니지만 total computable function
;; primitive recursive function은 항상 total computable function이다.
;; A(x, y)
;; 0                      if y = 0
;; 2 * y                  if x = 0
;; 2                      if y = 2
;; A(x - 1, A(x, y - 1))
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 0 n) : (* 2 n)
;; (A 1 n) : (exp 2 n)
;; (A 2 n) : n = 1 이면 1
;;           n > 1 이면 (exp 2 (exp 2 (exp 2 ... 2))) -> n번 반복
;; (A 3 n) : n = 1 이면 1
;;           n > 1 이면 ???


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1.2.2
;; 피보나치 수열 f(n) = f(n-1) + f(n-2), f(0) = 0, f(1) = 1

;; recursive function
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

;; iterative function
(define (fib-2 n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

;; 돈 바꾸는 방법 ???
;; 받은 돈을 동전으로 바꾸는 방법
;; 10 센트
;; 1 센트 * 10
;; 5 센트 * 2
;; 5 센트 + 1 센트 * 5

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds)
  (cond ((= kinds 1) 1)
        ((= kinds 2) 5)
        ((= kinds 3) 10)
        ((= kinds 4) 25)
        ((= kinds 5) 50)))

;; ex 1.11
(define (f-1 n)
  (if (< n 3)
      n
      (+ (f-1 (- n 1))
         (* 2 (f-1 (- n 2)))
         (* 3 (f-1 (- n 3))))))

(define (f-2 n)
  (define (iter a b c cnt)
    (if (= cnt 0)
        c
        (iter (+ a (* 2 b) (* 3 c)) a b (- cnt 1))))
  (iter 2 1 0 n))

;; ex 1.12
;; pascal's triangle,
;; p(r, c) = p(r-1, c-1) + p(r-1, c)
;; p(r, 1) = 1, p(r, r) = 1
(define (pt r c)
  (cond ((or (= c 1) (= r c)) 1)
        ((or (< c 0) (< r 0) (> c r)) 0)
        (else (+ (pt (- r 1) (- c 1))
                 (pt (- r 1) c)))))


;; 1.2.3

;; ex 1.14
;; ... ???

;; ex 1.15

;; a : p 프로시저 호출 회수
;; (sine 12.15)
;; (p (sine 4.5))
;; (p (p (sine 1.5)))
;; (p (p (p (sine 0.5))))
;; (p (p (p (p (sine 0.1666... )))))
;; (p (p (p (p (p (sine 0.0555...))))))
;; p 프로시저는 5번 호출된다

;; b : 기억공간과 계산단계의 자람 차수를 a함수로 나타내면
;; (floor (+ (/ (log a) (log 3)) 3))
;; 다른 솔루션에서는 (ceiling(/ (log (/ a 0.1)) (log 3))) ==> http://community.schemewiki.org/?sicp-ex-1.15


;; 1.2.4
(define (expt-1 b n)
  (if (= n 1)
      b
      (* b (expt-1 b (- n 1)))))

(define (expt-2 b n)
  (define (iter product b n)
    (if (= n 0)
        product
        (iter (* product b) b (- n 1))))
  (iter 1 b n))

(define (square x)
  (* x x))

;; fast version
;; n is even => b^n = (b ^ (n / 2))^2
;; n is odd  => b^n = b * b^(n-1)

(define (fastexpt-1 b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fastexpt-1 b (/ n 2))))
        ((odd? n) (* b (fastexpt-1 b (- n 1))))))

(define (fastexpt-2 b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          ((odd? n) (iter (* a b) b (- n 1)))))
  (iter 1 b n))

;; fastexpt-2의 실행시간이 log 비례인가??

;; ex 1.17

;; a * b, * 프로시저가 없다고 하고...

;; recursive
(define (prod-1 a b)
  (if (= b 0)
      0
      (+ a (prod-1 a (- b 1)))))

;; iterative
(define (prod-2 a b)
  (define (iter s a b)
    (if (= b 0)
        s
        (iter (+ s a) a (- b 1))))
  (iter 0 a b))

(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

;; fast *, recursive
(define (fast-prod-1 a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-prod-1 a (halve b))))
        ((odd? b) (+ a (fast-prod-1 a (- b 1))))))

;; ex 1.18

;; fast *, iterative
(define (fast-prod-2 a b)
  (define (iter sum a b)
    (cond ((= b 0) sum)
          ((even? b) (iter sum (double a) (halve b)))
          ((odd? b) (iter (+ sum a) a (- b 1)))))
  (iter 0 a b))


;; ex 1.19

;; T(pq)를 두 번 거듭 계산할 때 나온 값이 T(p'q')를 한 번 한것과 같다는 의미는
;; a2 = b1*q + a1*q + a1*p = b0*q' + a0(q' + p')
;; b2 = b1*p + a1*q        = b0*p' + a0*q'
;; 를 의미
;; a2와 b2를 p와 q에 대해 풀어보면,
;; a1 = b0*q + a0*q + a0*p = b0*q + a0*(q + p)
;; b1 = b0*p + a0*q
;; a2 = b1*q + a1*(q + p) = (b0*p + a0*p)*q + (b0*q + a0*(q + p))*(q + p)
;;    = b0*(q^2 + 2pq) + a0*(2q^2 + 2pq + p^2)
;; b2 = b1*p + a1*q = (b0*p + a0*q)*p + (b0*q + a0*(q + p))*q
;;    = b0*(p^2 + q^2) + a0*(2pq + q^2)
;; a2에서 p', q'로 확인하면
;; q' = q^2 + 2pq 이고,
;; q' + p' = q^2 + 2pq + p' = 2q^2 + 2pq + p^2 이므로,
;; p' = q^2 + p^2
;; 가 된다.
;; b2를 가지고 p'와 q'를 계산해도 같이 결과가 나옴을 확인할 수 있다.
;; 답
;; q' = q^2 + 2pq
;; p' = q^2 + p^2

(define (fib-8 n)
  (fib-iter-8 1 0 0 1 n))

(define (fib-iter-8 a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter-8 a b
                                   (+ (square q) (square p))
                                   (+ (square q) (* 2 p q))
                                   (/ count 2)))
        (else (fib-iter-8 (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))


;; 1.2.5

;; GCD, Euclid's Algorithm
(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))

;; ex 1.20
;; 정의대로 계산법
;; (GCD 206 40)
;; (if (= 40 0) ... )
;; (GCD 40 (remainder 206 40))
;; (if (= (remainder 206 40) 0) ... )
;; (if (= 6 0) ... )
;; (GCD (remainder 206 40) (remainder 40 (remainder 206 40)))
;; (if (= (remainder 40 (remainder 206 40)) 0) ... )
;; (if (= 4 0) ...)
;; (GCD (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; ...
;;
;; 인자먼저 계산법
;; (GCD 206 40)
;; (GCD 40 (remainder 206 40)) => (GCD 40 6)
;; (GCD 6 (remainder 40 6))    => (GCD 6 4)
;; (GCD 4 (remainder 6 4))     => (GCD 4 2)
;; (GCD 2 (remainder 4 2))     => (GCD 2 0)
;; => 2

;; prime number

;; method 1 - O(sqrt n)
;; n의 가장 작은 약수가 n이면 n은 소수이다.
;; n이 소수가 아니면 sqrt n보다 작거나 같은 약수가 반드시 존재한다.

(define (prime?-1 n)
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


;; method 2 - probability O(log n)
;; fermat 테스트
;;  - 주어진 n과 0 < a < n 인 임의의 a에 대해 a^n % n = a 이면, n은 소수
;; fermat 테스트는 확률적 방법이므로 여러번 반복한다
(define (prime?-2 n times)
  (cond ((= times 0) #t) ;; fermat 테스트를 times 횟수 반복했는데 모두 #t 인 경우
        ((fermat-test n) (prime?-2 n (- times 1))) ;; fermat 테스트를 통과한 경우
        (else #f))) ;; fermat 테스트를 통과하지 못한 경우

;; n^x 
;; x even (n^(x/2))^2
;; x odd  n*n^(x-1)
;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (square (expmod base (/ exp 2) m)) m))
;        (else
;         (remainder (* base (expmod base (- exp 1) m)) m))))

; time  - O(log n)
; space - O(n)

(e 10 20 3)
(r (s (e 10 10 3)) 3)
(r (s (r (s (e 10 5 3)) 3)) 3)
(r (s (r (s (* 10 (e 10 4 3))) 3)) 3)
(r (s (r (s (* 10 (r (s (e 10 2 3)) 3))) 3)) 3)
(r (s (r (s (* 10 (r (s (r (s (e 10 1 3)) 3)) 3))) 3)) 3)
(r (s (r (s (* 10 (r (s (r (s (r (* 10 (e 10 1 3)) 3)) 3)) 3))) 3)) 3)
(r (s (r (s (* 10 (r (s (r (s (r (* 10 (r (* 10 (e 10 0 3)) 3)) 3)) 3)) 3))) 3)) 3)
(r (s (r (s (* 10 (r (s (r (s (r (* 10 (r (* 10 1) 3)) 3)) 3)) 3))) 3)) 3)
(r (s (r (s (* 10 (r (s (r (s (r (* 10 (r 10 3)) 3)) 3)) 3))) 3)) 3)
(r (s (r (s (* 10 (r (s (r (s (r (* 10 1) 3)) 3)) 3))) 3)) 3)
(r (s (r (s (* 10 (r (s (r (s (r 10 3)) 3)) 3))) 3)) 3)
(r (s (r (s (* 10 (r (s (r (s 1) 3)) 3))) 3)) 3)
(r (s (r (s (* 10 (r (s (r 1 3)) 3))) 3)) 3)
(r (s (r (s (* 10 (r (s 1) 3))) 3)) 3)
(r (s (r (s (* 10 (r 1 3))) 3)) 3)
(r (s (r (s (* 10 1)) 3)) 3)
(r (s (r (s 10) 3)) 3)
(r (s (r 100 3)) 3)
(r (s 1) 3)
(r 1 3)
1


(define (fastexpt-2 b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          ((odd? n) (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(remainder (fastexpt-2 4 6) 3)
(iter 1 4 6)
(iter 1 16 3)
(iter 16 16 2)
(iter 16 256 1)
(iter 4096 256 0)
4096


(define (expmod base exp m)
  (remainder (fastexpt-2 base exp) m))

;; space O(1)
;; time  O(log n)
(remainder (fastexpt-2 2 2) 3)
(remainder (iter 1 2 2) 3)
(remainder (iter 1 4 1) 3)
(remainder (iter 4 4 0) 3)
(remainder 4 3)



(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ (random (- n 1)) 1)))

;; ex 1.21
; (smallest-divisor 199) => 199
; (smallest-divisor 1999) => 1999
; (smallest-divisor 19999) => 7

;; 1.22
(define (timed-prime-test n)
  (display n)
  (newline)
  (start-prime-test n (current-seconds)))

(define (start-prime-test n start-time)
  (if (prime?-1 n)
      (report-prime (- (current-seconds) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

