
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)
;; -> error, y를 얻을 때 아직 정의되지 않은 dy가 필요하기 때문

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (let ((integrand (force delayed-integrand)))
		   (add-streams (scale-stream integrand dt)
				int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
(show-stream (solve (lambda (y) y) 1 0.001))

;; ex 3.77
;; 사실 이해하고 풀었다기 보다는, 위의 패턴 그대로 적용한 거임
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral (delay (stream-cdr integrand))
				   (+ (* (stream-car integrand) dt)
				      initial-value)
				   dt)))))
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
(stream-ref (solve (lambda (y) y) 1 0.001) 1000) ;; > 2.7169239322359


;; ex 3.78
;; 음...
(define (solve-2nd a b y0 dy0 dt dydt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dydt))
  (define ddy (add-streams
	       (scale-stream dy (* -1 a))
	       (scale-stream y (* -1 b))))
  y)
;; (solve-2nd -1 2 1 -4 0.001 0.001)


;; ex 3.79
;; 음...
(define (solve-2nd f y0 dy0 dt dydt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dydt))
  (define ddy (stream-map f dy y))
  y)
;; (solve-2nd (lambda (dy y) (+ (* dy -1) (* y 2))) 1 -4 0.001 0.001)
