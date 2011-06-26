;; 2011.6.26
;; 3.4.2 병행성 다스리기

(define (make-serializer) '())
(define (parallel-execute . proc) '())

;; ex 3.39
(define x 10)
(define s (make-serializer))
(parallel-execute (lambda () (set! x ((s (lambda () (* x x)))))) ;; p1
                  (s (lambda () (set! x (+ x 1)))))              ;; p2
;; 101 : 발생 가능
;; 121 : 발생 가능
;; 110 : p1에서 (* x x) 프로세스는 줄세움 되어 있으므로 발생하지 않음.
;; 11  : P2 프로세스 전체가 줄세움 되어 있으므로 발생하지 않음.
;; 100 : (* x x) 결과를 set! 하는 프로세스는 줄세움 되어 있지 않으므로 발생 가능.

;; ex 3.40
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))    ;; P1
                  (lambda () (set! x (* x x x)))) ;; P2
;; 발생 가능한 것들
;; 1000000 : P1이 x를 100으로 놓은 다음, P2가 100 * 100 * 100 계산
;; 1000000 : P2가 x를 1000 놓은 다음, P2가 1000 * 1000 계산
;; 10000   : P1이 (* x x) 하려고 x 값을 두 번 가져오는 사이 p2는 x값을 10에서 1000으로 바꾼다.
;; 100000  : P2가 (* x x x) 할 때 처음 x 값 10 가져오고, P1이 계산되어 다음 x들은 100이 된다.
;; 10000   : P2가 (* x x x) 할 때 두번째 x까지는 10을 가져오고, 이후에 P1이 계산되어 마지막 x는 100이 된다.
;; 1000    : p1가 완료되었지만, p2가 이전에 가져온 x 10으로 계산한다.
;; 100     : p2가 완료되었지만, p1이 이전에 가져온 x 10으로 계산한다.

;; 아래와 같이 줄세움 하면 나오는 것은?
(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))    ;; P1
                  (s (lambda () (set! x (* x x x))))) ;; P2
;; 위의 발생 가능한것들 중 맨위 2가지만 가능.
;; 계산된 값은 항상 1000000 이 됨.

;; ex 3.41
;; balance를 줄세움 하지 않아도 문제되지 않음. 동의하지 않는다.
;; balance가 number가 아닌 물체라면?? 마찬가지일듯.???

;; ex 3.42
;; 새로운 프로시저에서는 make-account가 실행될때 줄세움이 된다.
;; 처음 balance 값으로 줄세움 되는거인가?
;; 모르겠당.

