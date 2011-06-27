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
            ;(else (error "Unknown request -- MAKE-ACCOUNT" m))
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
; 101, 121, 11 : 총 3가지
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

