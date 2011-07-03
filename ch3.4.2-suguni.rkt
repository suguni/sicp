#lang racket

(require (planet "sicp-concurrency.ss"
                 ("dyoo" "sicp-concurrency.plt" 1 1)))

;; 3.4.2 병행성 다스리기

;; (define (make-serializer) '())
;; (define (parallel-execute . proc) '())

(define (p-c1)
  (let ((x 10))
    (parallel-execute (lambda () (set! x (* x x)))
                      (lambda () (set! x (+ x 1))))
    (print x)
    (newline)))

;; ex 3.39
(define (p-case1)
  (define x 10)
  (define s (make-serializer))
  (parallel-execute (lambda () (set! x ((s (lambda () (* x x)))))) ;; p1
                    (s (lambda () (set! x (+ x 1)))))              ;; p2
  (print x)
  (newline))

;; 101 : 발생 가능
;; 121 : 발생 가능
;; 110 : p1에서 (* x x) 프로세스는 줄세움 되어 있으므로 발생하지 않음.
;; 11  : P2 프로세스 전체가 줄세움 되어 있으므로 발생하지 않음. - 삑!!!
;;       p2가 serialized 여도, p1의 (set! x value)가 serialized 아니므로 동시 발생 가능 
;;       발생 가능
;; 100 : (* x x) 결과를 set! 하는 프로세스는 줄세움 되어 있지 않으므로 발생 가능.

;; ex 3.40
(define (p-case2)
  (define x 10)
  (parallel-execute (lambda () (set! x (* x x)))    ;; P1
                    (lambda () (set! x (* x x x)))) ;; P2
  (print x)
  (newline))

;; 발생 가능한 것들
;; 1000000 : P1이 x를 100으로 놓은 다음, P2가 100 * 100 * 100 계산
;; 1000000 : P2가 x를 1000 놓은 다음, P2가 1000 * 1000 계산
;; 10000   : P1이 (* x x) 하려고 x 값을 두 번 가져오는 사이 p2는 x값을 10에서 1000으로 바꾼다.
;; 100000  : P2가 (* x x x) 할 때 처음 x 값 10 가져오고, P1이 계산되어 다음 x들은 100이 된다.
;; 10000   : P2가 (* x x x) 할 때 두번째 x까지는 10을 가져오고, 이후에 P1이 계산되어 마지막 x는 100이 된다.
;; 1000    : p1가 완료되었지만, p2가 이전에 가져온 x 10으로 계산한다.
;; 100     : p2가 완료되었지만, p1이 이전에 가져온 x 10으로 계산한다.

;; 아래와 같이 줄세움 하면 나오는 것은?
(define (p-case3)
  (define x 10)
  (define s (make-serializer))
  (parallel-execute (s (lambda () (set! x (* x x))))    ;; P1
                    (s (lambda () (set! x (* x x x))))) ;; P2
  (print x)
  (newline))

;; 위의 발생 가능한것들 중 맨위 2가지만 가능.
;; 계산된 값은 항상 1000000 이 됨.

;; ex 3.41
;; balance를 줄세움 하지 않아도 문제되지 않음. 동의하지 않는다.
;; balance가 number가 아닌 물체라면?? 마찬가지일듯.???

;; ex 3.42
;; 새로운 프로시저에서는 make-account가 실행될때 줄세움이 된다.
;; 처음 balance 값으로 줄세움 되는거인가?
;; 차이 없는거 같은데.. 모르겠당.

;; p399 여러 자원을 함께 쓰는 문제

;; ex 3.43
;; ???

;; ex 3.44
;; 1. Louis가 옳은가?
;;   틀리다. 이 프로시저는 문제가 없다.
;; 2. transfer와 exchange와의 차이점은?
;;   exchange의 경우 account1과 account2의 balance를 가져와 차이를 계산한 후 이를 각 계정에 반영한다.
;;   즉, p400의 문제가 발생할 수 있다.
;;   transfer의 경우 중간 계산 과정이 없으므로, 각 계정의 withdraw와 deposit의 원자성만 보존되면 된다.

;; ex 3.45
;; serialized-exchange 에서 어떤 serializer로 serialize 한 exchange를 실행하는데,
;; exchange 내에서 동일한 serilizer로 serialize 한 deposit 또는 withdraw를 실행하게 된다.
;; 이 상황에서는 exchange가 완료되지 않은 상태라 deposit 또는 withdraw를 실행할 수 없고,
;; exchange 프로세스가 종료되지 않게 된다.

(define (make-account-and-serialize balance)
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
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (#t (error ("Unknown request -- MAKE-ACCOUNT" m)))))
    dispatch))

(define (exchange acc1 acc2)
  (let ((diff (- (acc1 'balance)
                 (acc2 'balance))))
    ((acc1 'withdraw) diff)
    ((acc2 'deposit) diff)))

(define (serialized-exchange acc1 acc2)
  (let ((s1 (acc1 'serializer))
        (s2 (acc2 'serializer)))
    ((s1 (s2 exchange)) acc1 acc2)))

;; (define a1 (make-account-and-serialize 100))
;; (define a2 (make-account-and-serialize 200))
;; (exchange a1 a2)
;; 잠깜만..

;; p404 줄 세우개 만들기
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

;; ex 3.46
;; test-and-set! 이 알갱이 연산으로 만들어지지 않으면?
;; 1. (car cell)
;; 2. (set-car! cell true)
;; 두 개의 프로세스가 동시에 실행되면 두 프로세스 모두 mutex를 얻게될 수 있다.

;; ex 3.47
;; ???

;; p408 엇걸림

;; ex 3.48
;; 아래의 경우 엇걸림이 발생할 수 있다.
(define (deadlock-text)
  (define account1 (make-account 100))
  (define account2 (make-account 200))
  (parallel-execute (lambda () (serialized-exchange account1 account2))
                    (lambda () (serialized-exchange account2 account1))))
;; 이를 해결하려면 serialized-exchange 내부에서 account 인자 위치에 상관없이 동일한 방식으로
;; serialized 되도록 하면 된다. 이를 위해 account 생성시 고유한 번호를 붙이고,
;; serialized-exchange에서 작은 번호의 account의 serializer를 먼저 가져와 exchange를 serialize 되도록 하면 된다.

;; 대략 아래와 같은 코드가 될 듯.
(define (serialized-exchange-an account1 account2)
  (let ((n1 (account1 'number))
        (n2 (account2 'number))
        (s1 (account1 'serializer))
        (s2 (account2 'serializer)))
    (if (< n1 n2)
        ((s1 (s2 exchange)) account1 account2)
        ((s2 (s1 exchange)) account1 account2))))

;; make-account 는 아래처럼 수정
(define make-account
  ((lambda ()
    (let ((number 0))
      (define (make-account balance)
        (let ((acc-num number))
          (set! number (+ number 1))
          (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                       balance)
                "Insufficient funds"))
          (define (deposit amount)
            (set! balance (+ balance amount))
            balance)
          (let ((balance-serializer (make-serializer)))
            (define (dispatch m)
              (cond ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    ((eq? m 'balance) balance)
                    ((eq? m 'serializer) balance-serializer)
                    ((eq? m 'number) acc-num)
                    (#t (error ("Unknown request -- MAKE-ACCOUNT" m)))))
            dispatch)))
        make-account))))

;; ex 3.49
;; ???