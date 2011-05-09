#lang racket

;; 3.1 Assignment & Local state
;; 3.1.1 local state variable

;; p288
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin
        (set! balance (- balance amount)) ;; set!은 명령이다.
        balance)
      "Insufficient funds"))

;; [???]
;; set! 으로 하지말고 define 하면 어떻게 되는가?
;; (define x 100)
;; (define x (- x 50))
;; 이것도 덮어쓰기 아닌가?
;;
;; =>
;;  ...
;;  (begin
;;    (set! balance (- balance amount))
;;    balance)
;;  ...
;; define: not allowed in an expression context 에러 난다.

;; begin
;; 아래랑 마찬가지다. 제일 마지막 식을 반환한다.
;; (define (f a)
;;   (define (g x) (+ x 1))
;;   (define (h y) (+ (g 5) 3))
;;   (h (* a 2)))
;; 위 식은 아래와 동일하다.
;; (define (f a)
;;   (begin
;;     (define (g x) (+ x 1))
;;     (define (h y) (+ (g 5) 3))
;;     (h (* a 2))))


;; p289
;; 문제점: balance가 밖으로 드러나 있다.

;; p290 - 갇힌 변수로 만들기
;; 문제점: 처음에 무조건 100이 된다. - balance를 인자로 받아서 설정할 수 있게 만들어야 한다.
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))))

;; p291
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds")))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

;; balance로 인해 갇힌 환경이 생기고, 내부 lambda 프로시저에서만 balance를 이용할 수 있다.

;; p292
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  ;; message passing
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)


;; ex 3.1 - make-accumulator 만들기
(define (make-accumulator init)
  (let ((items (list init)))
    (lambda (v)
      (set! items (cons v items))
      (apply + items))))

(define A (make-accumulator 5))

;; ex 3.2 - make-monitored 만들기
(define (make-monitored f)
  (let ((c 0))
    (lambda (m)
      (cond ((eq? m 'how-many-calls?) c)
            ((eq? m 'reset-count) (begin (set! c 0) 0))
            (else (begin
                    (set! c (+ c 1))
                    (f m)))))))

(define s (make-monitored sqrt))
;; (s 100) ;; => 10
;; (s 'how-many-calls?) ;; => 1

;; ex 3.3 - 암호걸린 make-account
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
    (if (eq? pwd in-pwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        ;; "Incorrect password"를 답하는게 아님. 
        ;;(error "Incorrect password" in-pwd)))
        
        ;; 무조건 "Incorrect password"를 답하는 프로시저를 반환함
        (lambda (v) "Incorrect password")))
  
  dispatch)

(define acc1 (make-account-pwd 100 'secret-password))

;; ex 3.4 - 암호 + 경찰불러 make-account
(define (make-account-pwd-cops balance pwd)
  
  (define (call-the-cops) "POLICE")
    
  (let ((incorrect-count 0))
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
      (if (eq? pwd in-pwd)
          (begin
            ;; 성공하면 reset count
            (set! incorrect-count 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT" m))))
          (begin
            ;; 실패하면 count++
            (set! incorrect-count (+ incorrect-count 1))
            (lambda (v)
              ;; count가 7초과면 call-the-cops 호출
              (if (> incorrect-count 7)
                  (call-the-cops)
                  (list "Incorrect password" incorrect-count))))))
    
    dispatch))

(define accp1 (make-account-pwd-cops 100 's1))
