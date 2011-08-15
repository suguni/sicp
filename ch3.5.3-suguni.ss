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
;; i <= j 조건 없는 정수 쌍 스트림
(define (pairs-all s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (pairs-all (stream-cdr s) (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t)))
		 (stream-cdr s))))))

(define (pairs-all2 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t)))
		 (stream-cdr s))
     (pairs-all2 (stream-cdr s) (stream-cdr t))))))

  ;; (interleave
  ;;  (stream-map (lambda (x) (list (stream-car s) x)) t)
  ;;  (pairs-all (stream-cdr s) t))

(define sub-b (stream-filter (lambda (pair) (> (car pair) (cadr pair)))
			      (pairs-all integers integers)))


(define sub-b2 (stream-filter (lambda (pair) (> (car pair) (cadr pair)))
			      (pairs-all2 integers integers)))

;; 맞는건가??? 무한 시퀀스이므로 검증할 수 없음.

;; ex 3.68
;; pairs를 아래와 같이 정의하면?
(define (pairs-2 s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x)) t)
   (pairs-2 (stream-cdr s) (stream-cdr t))))
;; 첫번째 행과 대각만 가져온다. X

;; pairs-2 실행하면 무한 반복된다.
;; pairs의 경우 항상 cons-stream으로 묶어주므로
;; pairs를 재귀 호출되더라도 바로 stream를 얻어올 수 있는데 반해
;; 위의 경우는 stream-car를 얻기 위해 다시 interleave를 실행해야 하고,
;; 이러면 다시 pairs-2가 호출된다.
;; interleave는 special form이 아니므로 프로시저에 전달되기 전에 평가된다.
;; 무한 스트림은 stream-cdr의 평가가 지연되는 것만을 뜻하므로 위의 상황과는 관련 없다.

;; ex 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (pair) (cons (stream-car s) pair))
		(pairs t (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
;; 모르겠어서... http://wqzhang.wordpress.com/2009/08/22/sicp-exercise-3-69/

(define int-triples (triples integers integers integers))

(define pythagorean-triples
  (stream-filter (lambda (triple)
		   (let ((i (car triple))
			 (j (cadr triple))
			 (k (caddr triple)))
		     (= (+ (* i i) (* j j)) (* k k))))
		 int-triples))

;; ex 3.70
;; 쌍의 순서를 지정할 수 있는 pairs 프로시저를 짜라.
;; 쌍을 합칠 때 3.56의 merge 프로시저를 변형한 프로시저를 쓰면 된다.
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< (weight s1car) (weight s2car))
		  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
		 ((>= (weight s1car) (weight s2car))
		  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))))))))
		 ;; (else
		 ;;  (cons-stream s1car
		 ;; 	       (merge-weighted (stream-cdr s1)
		 ;; 			       (stream-cdr s2)
		 ;; 			       weight))))))))

;; a. 무게 함수가 i+j 인 것.
(define int-pairs-1 (weighted-pairs integers integers
				    (lambda (pair)
				      (+ (car pair) (cadr pair)))))
;; 이렇게 정의하면 (1 1), (1 2), (1 3), ... 으로 만들어진다. X
;; (1 3)과 (2 2)가 동일한 무게이므로 (2 2)를 무시하게 된다. X
;; merge-weighted 함수를 잘못 만든 것인가? X
;; 기존 merge의 경우 값이 동일한 경우 두 stream에서 모두 제거하고 다시 merge하는데,
;; 위와 같이 변경하면 weight가 동일하더라도 모두 포함된다.

;; b. 2, 3, 5로 나눠떨어지지 않는 수의 쌍, 무게 함수는 2i + 3j + 5ij
(define int-pairs-2 
  (let ((values (stream-filter
		 (lambda (i) (and (not (= (remainder i 2) 0))
				  (not (= (remainder i 3) 0))
				  (not (= (remainder i 5) 0))))
		 integers)))
    (weighted-pairs values values
		    (lambda (pair)
		      (+ (* 2 (car pair))
			 (* 3 (cadr pair))
			 (* 5 (car pair) (cadr pair)))))))
;; 아래 순서임.
;; (1 1), (1 7), (1 11), (1 13), (1 17), (1 19), (1 23), (1 29), (1 31), (7 7), (1 37)  

;; ex 3.71
(define ramanujan-numbers
  ((lambda ()
    (define (cubic-sum pair)
      (+ (* (car pair) (car pair) (car pair))
	 (* (cadr pair) (cadr pair) (cadr pair))))

    (define (iter s)
      (let ((scar (stream-car s))
	    (scdr (stream-cdr s)))
	(let ((cv1 (cubic-sum scar))
	      (cv2 (cubic-sum (stream-car scdr))))
	  (if (= cv1 cv2)
	      (cons-stream (list cv1 scar (stream-car scdr))
			   (iter (stream-cdr scdr)))
	      (iter scdr)))))

    (iter (weighted-pairs integers integers cubic-sum)))))

;; 결과
;; (stream-ref ramanujan-numbers 5)
;; (39312 (15 33) (2 34))
;; (stream-ref ramanujan-numbers 4)
;; (32832 (18 30) (4 32))
;; (stream-ref ramanujan-numbers 3)
;; (20683 (19 24) (10 27))
;; (stream-ref ramanujan-numbers 2)
;; (13832 (18 20) (2 24))
;; (stream-ref ramanujan-numbers 1)
;; (4104 (9 15) (2 16))
;; (stream-ref ramanujan-numbers 0)
;; (1729 (9 10) (1 12))

;; ex 3.72
(define (ramanujan-triples)
  (define (cubic-sum pair)
    (+ (* (car pair) (car pair) (car pair))
       (* (cadr pair) (cadr pair) (cadr pair))))
  
  (define (iter s)
    (let ((scar (stream-car s))
	  (scdr (stream-cdr s))
	  (scddr (stream-cdr (stream-cdr s))))
      (let ((cv1 (cubic-sum scar))
	    (cv2 (cubic-sum (stream-car scdr)))
	    (cv3 (cubic-sum (stream-car scddr))))
	(if (= cv1 cv2 cv3)
	    (cons-stream (list cv1 scar (stream-car scdr) (stream-car scddr))
			 (iter (stream-cdr scddr)))
	    (iter scdr)))))

    (iter (weighted-pairs integers integers cubic-sum)))

;; 첫번째 값,
;; (87539319 (255 414) (228 423) (167 436))
;; 무자게 오래 걸린다. Macbook Pro 2GHz Intel i7에서 대략 5분 45초 걸림. ^^;

;; http://www.durangobill.com/Ramanujan.html 에 보면
;; Taxicab(4) is thus 6963472309248. The new version of the ramanujans.c program (see below) took 30 seconds to find Taxicab(4). (3GHz Pentium 4 running Windows XP) 
;; 라고 되어 있는데, 이것보다 한참은 느린듯.
;; 잘못 짠걸까?
