#lang racket

;; 데이터 중심 프로그래밍 : 2.4.2에서 cond로 tag를 확인해서 처리했는데 이 과정을 일반화된 연산을 이용하는 방식

;; contents에 tag 붙이기
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; 복소수를 직각 좌표로 표현하는 경우
(define (install-rectangular-package)
  ;; function
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a))
          (* r (sin a))))
  
  ;; utility
  (define (tag x) (attach-tag 'rectangular x))
  
  ;; interface
  (put 'real-part '(rectangular) real-part)
  ;; ...
  
  'done)

;; 복소수를 극 좌표로 표현하는 경우
(define (install-polar-package)
  ;; function
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))
  
  ;; utility
  (define (tag x) (attach-tag 'polar x))
  
  ;; interface
  (put 'real-part '(polar) real-part)
  ;; ...
  
  'done)

;; generic operator
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

;; 이런식으로 사용.
(define (real-part x) (apply-generic 'real-part z))

;; 2.4.2에서는 새로운 type이 추가될때마다 real-part, imag-part 등의 selector를 변경해야 했다.
;; 또 다른 타입에 대한 프로그램이 추가될때마다 이름이 겹치지 않는지 확인해서 수정이 필요했다.
;; 위와 같이 짜면 새로운 타입에 대한 프로그램을 짜는 사람만 고민하면 된다.

;; 근데 put와 get이 어떻게 동작하는지 잘 모르겠음. put의 type 인자가 리스트인데... 왜???
