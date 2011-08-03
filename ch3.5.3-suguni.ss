;; sqrt 의 stream 버전
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess) (sqrt-improve guess x))
			     guesses)))
  guesses)


;; pi의 stream 버전
;; pi/4 = 1 - 1/3 + 1/5 - 1/7 ...

(define (pi-summands n)
  (cons-stream (/ 1 n)
	       (stream-map - (pi-summands (+ n 2)))))
;; (1/1 (stream-map - (pi-summands 3)))
;; (1/1 (stream-map - (1/3 (stream-map - (pi-summands 5)))))
;; (1/1 (stream-map - (1/3 (stream-map - (1/5 (stream-map - (pi-summands 7)))))))
;; (1/1 -1/3 +1/5 ...)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (partial-sums stream)
  (define sums
    (cons-stream (stream-car stream)
		 (add-streams (stream-cdr stream)
			      sums)))
  sums)

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;; euler-transform
(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1)) (+ (- s0 (* s1 2)) s2)))
		 (euler-transform (stream-cdr s)))))

;; tableau
(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

(accelerated-sequence euler-transform pi-stream)



(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess) (sqrt-improve guess x))
			     guesses)))
  guesses)


;; pi의 stream 버전
;; pi/4 = 1 - 1/3 + 1/5 - 1/7 ...

(define (pi-summands n)
  (cons-stream (/ 1 n)
	       (stream-map - (pi-summands (+ n 2)))))
;; (1/1 (stream-map - (pi-summands 3)))
;; (1/1 (stream-map - (1/3 (stream-map - (pi-summands 5)))))
;; (1/1 (stream-map - (1/3 (stream-map - (1/5 (stream-map - (pi-summands 7)))))))
;; (1/1 -1/3 +1/5 ...)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (partial-sums stream)
  (define sums
    (cons-stream (stream-car stream)
		 (add-streams (stream-cdr stream)
			      sums)))
  sums)

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;; euler-transform
(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1)) (+ (- s0 (* s1 2)) s2)))
		 (euler-transform (stream-cdr s)))))

;; tableau
(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

(accelerated-sequence euler-transform pi-stream)


;; ex 3.63
;; 원래 sqrt-stream
(define (sqrt-stream-1 x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess) 
			       (sqrt-improve guess x))
			     guesses)))
  guesses)

;; (cons 1.0 (map improve guesses))
;; (cons 1.0 (map improve (cons 1.0 (map improve guesses))))
;; (cons 1.0 '((improve 1.0) (improve (improve 1.0)) ...))

;; 수정해본 sqrt-stream
(define (sqrt-stream-2 x)
  (cons-stream 1.0
	       (stream-map (lambda (guess) 
			     (sqrt-improve guess x))
			   (sqrt-stream-2 x))))

;; (cons 1.0 (map improve (sqrt-stream-2 x)))
;; (cons 1.0 (map improve (cons 1.0 (map improve (cons 1.0 (map improve ...))))))

;; sqrt-stream-2는 재귀될때마다 새로운 stream을 만들어 내어
;; 동일한 값임에도 앞서 계산된 값을 이용하지 못하고 새롭게 계산해야 되서
;; 효율성이 떨어진다.

