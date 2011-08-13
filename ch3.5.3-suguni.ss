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


;; ex 3.64
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; 별로 이쁘지가 않은데...
(define (stream-limit stream limit)
  (if (< (abs (- (stream-car (stream-cdr stream))
		 (stream-car stream)))
	 limit)
      (stream-car stream)
      (stream-limit (stream-cdr stream) limit)))


;; ex 3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln2-summands (+ n 1)))))

(define ln2 (partial-sums (ln2-summands 1)))
(define ln2-et (euler-transform ln2))
(define ln2-as (accelerated-sequence euler-transform ln2))

;; ln2-as는 9번째만 되도, 소숫점 15자리까지 맞는 값이 나온다.
;; 10번째 부터는 divide by 0 에러. 위에서 사용한 pi에서도 그러함.
;; euler-transform에 (+ (- s0 (* s1 2)) s2) 식이 있는데
;; s0, s1, s2 가 모두 같은 값이 나오면서 분모가 0라 되는 것임.

