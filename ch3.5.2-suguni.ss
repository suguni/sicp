;; UCB Scheme(STk)로 풀이합니다.

(define (integers)
  (integers-starting-from 1))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (divisible? x y)
  (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (divisible? x 7))
		 (integers)))

(stream-ref no-sevens 10) ;; > 77

;; fibonacci 수열
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

;; fibs> 0 1 1 2 3 5 8 13 21 34 55 89
(stream-ref fibs 10) ;; > 55

;; 에라토스테네스의 체를 이용한 소수 계산
(define (sieve stream)
  (cons-stream (stream-car stream)
	       (sieve
		(stream-filter (lambda (x)
				 (not (divisible? x (stream-car stream))))
			       (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50) ;; > 233

;; p427 스트림을 드러나지 않게 정의하는 방법

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

;; fibonacci 수열을 위처럼 정의하기
(define fibs
  (cons-stream 0
	       (cons-stream 1
			    (add-streams (stream-cdr fibs)
					 fibs))))

;; scale-stream
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

;; ex 3.53. 아래식의 스트림 원소를 적어보기
(define s (cons-stream 1 (add-streams s s)))
;; (1 2 4 8 16 32 ... ) 1부터 2배씩 증가하는 수열

;; ex 3.54
;; mul-stream 정의하기
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; factorials 정의하기 > 1 * 2 * 3 * 4 * 5 * ...
;;                     > 1 2 6 24 120 720 ...  
(define factorials
  (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

;; ex 3.55 partial-sums 프로시저 짜기
;; (partial-sums integers) > 1 3 6 10 15
(define (partial-sums stream)
  (define sums
    (cons-stream (stream-car stream)
		 (add-streams (stream-cdr stream)
			      sums)))
  sums)

;; ex 3.56
;; 2, 3, 5 외의 소수 인수를 가지지 않는 양의 정수
;; 아래것들에 해당
;; 2 3 4 5 6 8 9 10 12 15 16 18 20 24 ...
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (scale-stream S 2)
				(merge (scale-stream S 3)
				       (scale-stream S 5)))))

;; ex 3.57
;; pass

;; ex 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; quotient는 몫을 반환하는 프로시저
(quotient 5 2)  ;; > 2
(quotient 10 3) ;; > 3

;; remainder는 나머지를 반환하는 프로시저
(remainder 5 2) ;; > 1

;; (expand 3 8 10) > (3 7 5 0 0 0 ... )
;; 3 / 8 = 0.3750
;; (expand 1 7 10) > (1 4 2 8 5 7 1 4 2 8 5 7 1 ... )
;; 1 / 7 = 0.1428571428571...

;; 해석
;; radix(진수)에서의 num / den

;; ex 3.59
(define (over-series-from-n n)
  (cons-stream (/ 1 n) (over-series-from-n (+ n 1))))

(define over-series (over-series-from-n 1))

(define (integrate-series coeffs)
  (mul-streams coeffs over-series))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;; derive sine > cosine
;; derive cosine > - sine

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0  (integrate-series cosine-series)))

(stream-ref cosine-series 0) ;;  1
(stream-ref cosine-series 2) ;; -0.5
(stream-ref cosine-series 4) ;;  0.0416667

(stream-ref sine-series 1) ;;  1
(stream-ref sine-series 3) ;; -0.16667
(stream-ref sine-series 5) ;;  0.00833

;; ex 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (mul-series s1 (stream-cdr s2))
			    (scale-stream (stream-cdr s1) (stream-car s2)))))

;; sin^2(x) + cos^2(x) = 1
(define test
  (add-streams (mul-series sine-series sine-series)
	       (mul-series cosine-series cosine-series))) ;; > (1 0 0 0 ... )

;; ??? 정확한건지???
;; 실제로는 
;; (stream-ref test 4) >> 5.55111512312578e-17
;; (stream-ref test 16) >> -2.06795153138257e-25

;; ex 3.61 -- how to check???
(define (invert-unit-series s)
  (cons-stream 1
	       (scale-stream (mul-series (stream-cdr s)
					 (invert-unit-series s))
			     -1)))

;; ex 3.62
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "divide by 0" "DIV-SERIES")
      (mul-series s1 (invert-unit-series s2))))

;; tan x = (sin x) / (cos x)
(define tangent-series (div-series sine-series cosine-series))
