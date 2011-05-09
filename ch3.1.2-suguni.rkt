#lang racket

;; ch3support.scm
(define (rand-update x)
  (let ((a 27)
        (b 26)
        (m 127))
    (modulo (+ (* a x) b) m)))
;; x2 = (rand-update x1)
;; x3 = (rand-update x2)
;; 인자(x)의 의미는 무엇인가? 왜 이런식으로 random 함수를 만들까?
;; -> p295 각주 설명 참조. pseudo random number 이라 그러함.
;;    일정 범위 안의 고른 분포를 가진 수열 값을 반환하도록 만든 것임

;; 질문! 위의 modulo 연산을 썼을때 같이 값이 반복될 가능성은 없는가?
;;       (rand-update x) => x 가 되는 경우

;; p296
(define random-init 7)
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;; rand-update에서는 다음에 random값을 얻기 위해 이전 값을 알고 있어야 한다.
;; rand에서는 내부에 상태를 변하면서 가지고 있으므로 쓰는 쪽에서는 아무것도 몰라도 된다.


;; p297
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

;; p297 - rand 사용 안하는 경우 - 모듈화가 전혀 안되어 있다.
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1) (+ trials-passed 1) x2))
              (else
               (iter (- trials-remaining 1) trials-passed x2))))))
  (iter trials 0 initial-x))

(define (estimate-pi-2 trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

;; ex 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
;; 갑자기 random 프로시저 등장.. ㅎㅎ

(define (estimate-integration x1 x2 y1 y2 trials)
  (define (pred)
    (let ((radius (/ (- x2 x1) 2.0)))
      (let ((center-x (+ x1 radius))
            (center-y (+ y1 radius))
            (x (random-in-range x1 x2))
            (y (random-in-range y1 y2)))
        (>= (sqr radius) (+ (sqr (- x center-x)) (sqr (- y center-y)))))))
  (let ((area (* 1.0 (- x2 x1) (- y2 y1))))
    (* (monte-carlo trials pred) area)))
;; (estimate-integration 0 10 0 10 99)
;; 책에서는 estimate-integration 프로시저가 p, x1, x2, y1, y2, trials 를 받는다고 했는데..
;; 위에서는 p(predicate)를 내부에서 정의했음.

;; ex3.6
;; (rand 'generate) - 생성
;; ((rand 'reset) new-value) - 리셋하고 new-value에서 시작
(define rand-2
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) (begin (set! x (rand-update x)) x))
            ((eq? m 'reset) (lambda (init)
                              (set! x (rand-update init))
                              x))))))
