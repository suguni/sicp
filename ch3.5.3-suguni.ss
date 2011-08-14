;; test
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

;; p441 쌍으로 이루어진 무한 스트림

(define (prime-sum-pair)
  (stream-filter (lambda (pair)
		   (prime? (+ (car pair) (cadr pair))))
		 int-pairs))
;; int-pairs는 i<=j 인 모든 정수 쌍(i, j) 차례열

;; (define (square x)
;;   (* x x))

;; (define (divides? a b)
;;   (= (remainder b a) 0))

;; (define (smallest-divisor n)
;;   (find-divisor n 2))

;; (define (find-divisor n test-divisor)
;;   (cond ((> (square test-divisor) n) n)
;; 	((divides? test-divisor n) test-divisor)
;; 	(else (find-divisor n (+ test-divisor 1)))))

;; (define (prime? n)
;;   (= n (smallest-divisor n)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;; (define int-pairs
;;   (stream-filter (lambda (pair) (<= (car pair) (cadr pair)))
;; 		 (stream-map (lambda (x y) (list x y)) (integers) (integers))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; 이렇게 정의하면 s1이 무한이므로 s2값은 하나도 못가져온다.
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (stream-append (stream-cdr s1) s2))))

;; 이렇게 가져와야 s1과 s2가 섞여서 합쳐진다. 위의 pairs에서도 interleave를 사용해야 한다.
;; 문제: 순서가 의미가 있는경우... 그 순서를 어떻게 표현할 것인가?
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(define int-pairs (pairs integers integers))

;; ex 3.66
;; int-pairs를 위에서 세 조각으로 구성한 방식으로 보면,
;; 1,1 - 1
;; 1,n - 짝수번째, (n-1)*2 번째 쌍
;; 기타 (s, t) - 홀수번째이고 아래 수식으로 계산할 수 있다.
;; (define (idx s t)
;;   (let ((s1 (- s 1))
;; 	(t1 (- t 1)))
;;     (+ (- (* (+ (/ (* t1 (- t1 1)) 2) s1)
;; 	     2)
;; 	  1)
;;        2)))
;; 합쳐서 생각해보면
(define (pair-index s t)
  (cond ((and (= s 1) (= t 1)) 1)
	((= s 1) (* (- t 1) 2))
	(else 
	 (let ((s1 (- s 1))
	       (t1 (- t 1)))
	   (+ (- (* (+ (/ (* t1 (- t1 1)) 2) s1)
		    2)
		 1)
	      2)))))

;; ex 3.67