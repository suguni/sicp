(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
	  ((prime? count) (iter (+ count 1) (+ accum count)))
	  (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes a b)
  (accumulate +
	      0
	      (filter prime?
		      (enumerate-interval a b))))

;; (stream-car (cons-stream x y)) == x
;; (stream-cdr (cons-stream x y)) == y

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (stream-enumerate-interval (+ low 1) high))))

(stream-car
 (stream-cdr
  (stream-filter prime?
		 (stream-enumerate-interval 4 20))))
(stream-car
 (stream-cdr
  (stream-filter prime?
		 (cons 4 (delay (stream-enumerate-interval 5 20))))))

(stream-car
 (stream-cdr
  (stream-filter prime?
		 (cons 5 (delay (stream-enumerate-interval 6 20))))))
(stream-car
 (stream-cdr
  (cons-stream 5
	       (stream-filter prime?
			      (stream-enumerate-interval 6 20)))))
(stream-car
 (stream-cdr
  (cons-stream 5
	       (cons-stream 7
			    (stream-filter prime?
					   (stream-enumerate-interval 8 20))))))
(stream-car
 (cons-stream 7 (stream-filter ...)))

(stream-car
 (stream-cdr
  (stream-filter prime? (cons 4 (delay (stream-enumerate-interval 5 20))))))

		 (stream-enumerate-interval 10000 1000000))))


(stream-filter prime?
	       (stream-enumerate-interval 10000 100000000))

(stream-filter prime?
	       (cons 10000 (delay (stream-enumerate-interval 10001 1000000))))
;; prime? no
(stream-filter prime?
	       (cons 10001 (delay (stream-enumerate-interval 10002 1000000))))

(cons-stream 10001
	     (stream-filter prime? (stream-enumerate-interval 10002 1000000)))

(cons-stream 10001
	     (cons-stream 10007
			  (stream-filter prime? (stream-enumerate-interval 10008 1000000))))






;; (stream-enumerate-interval 10000 1000000)
;; => (cons-stream 10000 (stream-enumerate-interval 10001 1000000)
;; => (cons 10000 (delay (stream-enumerate-interval 10001 1000000)

(define (stream-filter pred? stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred? (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred? (stream-cdr stream))))
	(else (stream-filter pred? (stream-cdr stream)))))


;; 한가지 stream만 받는 stream-map
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr)))))

;; (map proc (a b c) (d e f) (g h i))
;; (apply proc (a d g)) > (proc a d g)

;; ex 3.50 여러개의 인자를 받을 수 있는 stream-map
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
	(apply proc (map stream-car argstreams))
	(apply stream-map
	       (cons proc (map stream-cdr argstreams))))))

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 (cons-stream 4 the-empty-stream)))))
(define s2 (cons-stream 10 (cons-stream 20 (cons-stream 30 (cons-stream 40 the-empty-stream)))))
(define s3 (cons-stream 100 (cons-stream 200 (cons-stream 300 (cons-stream 400 the-empty-stream)))))

(stream-ref (stream-map + s1 s2 s3) 3) ;; > 444

;; stream을 어떻게 만들어야 하는지 잘 모르겠음.

;; ex 3.51 
(define (show x)
  (display-line x)
  x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
;; 1. x가 정의될 때는 0이 출력된다. 첫번째 항목은 지연없이 실행된다.
;; 2. stream-ref가 호출되면 x가 정의될 때 실행된 0은 제외한 1부터 5까지가 출력된다.
;; 3. 다음번 stream-ref가 호출되면, 앞서 실행된 5는 빠진 6, 7이 출력된다.



;; ex 3.52

;; 보통 리스트이면... sum이 항상 210이다.
(define sum 0)
(define (accum x)
  (set! sum (+ sum x))
  sum)
(define seq (map accum (enumerate-interval 1 20)))
;; > (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
;; sum = 210
(define y (filter even? seq))
;; > (6 10 28 36 66 78 120 136 190 210)
;; sum = 210
(define z (filter (lambda (x) (= (remainder x 5) 0)) seq))
;; > (10 15 45 55 105 120 190 210)
;; sum = 210
(list-ref y 7)
;; 136
;; sum = 210
(display z)
;; > (10 15 45 55 105 120 190 210)
;; sum = 210

;; stream 이면... sum값이 변한다.
(define sum 0)
(define (accum x)
  (set! sum (+ sum x))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; > (1 . (delay ... ))
;; sum = 1
;; sum = 1 -- no-memo
(define y (stream-filter even? seq))
;; > (6 . (delay ... ))
;; sum = 6
;; sum = 1 + (1 + 3 + 6) = 11 -- no-memo
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
;; > (10 . (delay ...))
;; sum = 10
;; sum = 11 + (1 + 3 + 6 + 10) = 33 -- no-memo
(stream-ref y 7)
;; > 136
;; sum = 136
;; sum = 33 + (6 + 10 + 28 + 36 + 66 + 78 + 120 + 136) -- no-memo
(display-stream z)
(show-stream z 100) ;; (ss z 100)
;; > 10 15 45 55 105 120 190 210 'done
;; sum = 210
;; sum = 33 + (6 + 10 + 28 + 36 + 66 + 78 + 120 + 136) + (10 + 15 + 45 + 55 + 105 + 120 + 190 + 210) -- no-memo

;; memo-proc를 사용하지 않으면 stream-filter 실행될 때마다 accum이 실행되므로 sum이 계속 변경된다.

;; memo-proc을 사용하지 안은 경우 답은 신뢰할 수 없음... 좀 더 고민 필요!!!!
