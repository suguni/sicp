#lang racket

;; 3.1.3 덮어쓰기를 끌어들인 대가

;; p301
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W (make-simplified-withdraw 25))

(define (make-decrementer balance)
  (lambda (amount) (- balance amount)))
(define D (make-decrementer 25))

;; (D 20)
;; >> ((lambda (amount) (- 25 amount)) 20)
;; >> (-25 20)

;; (W 20)
;; >> ((lambda (amount) (set! balance (- 25 amount)) balance) 20)
;;    위 balance들은 값이 아닌 메모리(공간)를 나타내므로 맞바꿈이 안된다.
;;    즉, 맞바꿈 계산법으로 풀어지지 않는다.
;;    결론!!! 같은 물체를 같게 볼 수 없다.

;; p307
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* product counter) (+ counter 1))))
  (iter 1 1))

(define (factorial-i n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin
            ;; 아래 명령들이 순서가 바뀌면 프로그램이 잘못 돌아간다.
            (set! product (* counter product))
            (set! counter (+ counter 1))
            (iter))))
    (iter)))
;; imperative의 문제는 계산 순서에 영향에 미친다.
;; 시간에 영향을 받고 병행 환경에서는 문제가 생길 가능성이 높아진다.

;; ex 3.7

;; ex 3.3에서 만든 암호걸린 make-account
(define (make-account-pwd balance pwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (dispatch in-pwd m)
    ;; password를 확인하는 메시지 처리 추가
    (if (eq? m 'check-password)
        (eq? pwd in-pwd) ;; dispatch가 아닌데(프로시저를 반환하는게 아님)...
        (if (eq? pwd in-pwd)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT" m)))
            (error "Incorrect password" in-pwd))))
  dispatch)

(define peter-acc (make-account-pwd 100 'pwd1))

;; make-joint
(define (make-joint account acc-pwd pwd)
  (if (account acc-pwd 'check-password)
      (lambda (in-pwd m)
        (if (eq? in-pwd pwd)
            (account acc-pwd m)
            (error "Incorrect password" in-pwd)))
      (error "Incorrect password")))

(define paul-acc (make-joint peter-acc 'pwd1 'pwd2))
;; (define errr-acc (make-joint peter-acc 'pwdX 'pwd2))  >> error

;((peter-acc 'pwd1 'withdraw) 20) ;; >> 80
;((paul-acc 'pwd2 'withdraw) 20)  ;; >> 60
;((peter-acc 'pwd1 'deposit) 5)   ;; >> 65

;; error 프로시저는 Java의 exception과 같은 놈 아닌가?
;; 정상적인 flow에서 exception을 사용하는 것은 좋은 방법이 아님

;; ex 3.8
;; (+ (f 0) (f 1)) 의 결과가
;;  (f 0)를 먼저 계산하면 0
;;  (f 1)을 먼저 계산하면 1이 나오게 f를 짜기
(define f
  (let ((s 1))
    (lambda (n)
      (set! s (* s n))
      s)))

;; 부분식의 계산 순서를 바꿀 수 없으므로
;; 왼쪽에서 오른쪽으로 계산된다고 생각하고 아래 두번 실행해 봄.
;; 각각 실행하여 확인해야 함. 
;; (+ (f 0) (f 1)) ;; >> 0
;; (+ (f 1) (f 0)) ;; >> 1
