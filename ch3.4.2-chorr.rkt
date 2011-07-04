#lang racket
(require (planet "sicp-concurrency.ss" ("dyoo" "sicp-concurrency.plt" 1 1)))

;; ch 3.4.2 병행성을 다스리는 방법
(define (parallel-case1)
  (define x 10)
  (parallel-execute (lambda () (set! x (* x x)))
                    (lambda () (set! x (+ x 1))))
  (display x)
  (newline))

(define (parallel-case2)
  (define x 10)
  (define s (make-serializer))
  (parallel-execute (s (lambda () (set! x (* x x))))
                    (s (lambda () (set! x (+ x 1)))))  
  (display x)
  (newline))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (#t (error "Unknown request -- MAKE-ACCOUNT" m))
            ))
    dispatch))
  
;; ex 3.39
(define (parallel-case3)
  (define x 10)
  (define s (make-serializer))
  (parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                    (s (lambda () (set! x (+ x 1)))))
  (display x)
  (newline))
; 101, 121, 11, 100 : 총 4가지
; P1 에서 set! 하기 직전 P2 가 실행되어(x=10 으로 판단) 최종적으로 11 이라는 엉뚱한 결과를 보인다.

;; ex 3.40
; (define x 10)
; (parallel-execute (lambda () (set! x (* x x)))
;                   (lambda () (set! x (* x x x))))
;> 1000000, 1000, 100, 10000, 100000 : 총 5가지

; (define x 10)
; (define s (make-serializer))
; (parallel-execute (s (lambda () (set! x (* x x))))
;                   (s (lambda () (set! x (* x x x)))))
;> 항상 1000000

;; ex 3.41
(define (make-account-2 balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected (lambda () balance))))))
    dispatch))
;> 직접적인 balance 에 큰영향은 없지만, withdraw 또는 deposit 프로세스 중 balance를 확인하는 타이밍에 정상적이지 않은 (이전의) 값을 가져오게 된다.
;> 즉, balance 1000 상황에서 병행으로 withdraw 100 / balance 실행이되면 balance가 1000 그대로 보여질 수 있다.

;; ex 3.42
(define (make-account-3 balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
	  (protected-deposit (protected deposit)))
      (define (dispatch m)
	(cond ((eq? m 'withdraw) protected-withdraw)
	      ((eq? m 'deposit) protected-deposit)
	      ((eq? m 'balance) balance)))
      dispatch)))
;> make-serializer 구현에 대한 이해가 먼저 필요



;; 줄 세우개 만들기
(define (make-serializer-2)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (make-cell #f)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))


;; 여러 자원을 함께 쓰는 문제
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer-2)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) id)
            (#t (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(define acc1 (make-account-and-serializer 100 1))
(define acc2 (make-account-and-serializer 200 2))
(define acc3 (make-account-and-serializer 300 3))
;; deadlock
;(parallel-execute (lambda () (serialized-exchange acc1 acc2))
;                  (lambda () (serialized-exchange acc2 acc1)))


;; deadlock-avoidance
(define (serialized-exchange-2 account1 account2) 
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'id) (account2 'id))
        ((serializer2 (serializer1 exchange))
         account2
         account1)
        ((serializer1 (serializer2 exchange))
         account1
         account2))
    (display (cons (account1 'balance) (account2 'balance)))
    (newline)
    ))

(parallel-execute (lambda () (serialized-exchange-2 acc1 acc2))
                  (lambda () (serialized-exchange-2 acc2 acc1)))
