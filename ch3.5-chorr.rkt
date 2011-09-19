#lang planet neil/sicp

;; ch 3.5.1
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


(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred? stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred? (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred? (stream-cdr stream))))
	(else (stream-filter pred? (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;; ex 3.51
(define (show x)
  (display-line x)
  x)

;: (define x (stream-map show (stream-enumerate-interval 0 10)))
;: (stream-ref x 5)
;: (stream-ref x 7)

;; ex 3.52
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

;: (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;: (define y (stream-filter even? seq))
;: (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

;: (stream-ref y 7)
;: (display-stream z)



;; ch 3.5.2
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y)
  (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))


(define (sieve stream)
  (cons-stream 
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))


(define ones
  (cons-stream 1 ones))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
	(apply proc (map stream-car argstreams))
	(apply stream-map
	       (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers-by-add (cons-stream 1 (add-streams ones integers)))

(define fibs-by-add
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs-by-add)
                                         fibs-by-add))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;; ex 3.53
(define stm (cons-stream 1 (add-streams stm stm)))

;; ex 3.54
(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-stream factorials (integers-starting-from 2))))

;; ex 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))
;; ex 3.56
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
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

;(define S (cons-stream 1 (merge (scale-stream S 2) 
;                                (merge (scale-stream S 3)
;                                       (scale-stream S 5)))))

;; ex 3.57
; N(m) = N(m-1) + N(m-2)
; N(m-1), N(m-2) 상황을 매번 실행하게 되면 지수승 증가

;; ex 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; (stream-range (expand 1 7 10) 5)
; (stream-range (expand 3 8 10) 5)

(define (stream-range s n)
  (display "stream : ")
  (display (stream-car s))
  (newline)
  (if (= n 0)
      (stream-car s)
      (stream-range (stream-cdr s) (- n 1))))

;; ex 3.59
;; a)
;(define (integrate-series c)
;  (mul-stream c over-series))
;; b)
;(define exp-series
;  (cons-stream 1 (integrate-series exp-series)))

;; ex 3.60


;; ch 3.5.3

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
; (display-stream (sqrt-stream 2))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
; (display-stream pi-stream)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
; (display-stream (euler-transform pi-stream))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
; (display-stream (accelerated-sequence euler-transform pi-stream))

;; ex 3.63
(define (sqrt-stream-ex x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream-ex x))))

;; ex 3.64
(define (sqrt-t x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; source : http://wqzhang.wordpress.com/2009/08/12/sicp-exercise-3-64/
(define (stream-limit s tolerance)
  (let ((s2 (stream-cdr s)))
    (if (< (abs (- (stream-car s)
                   (stream-car s2)))
           tolerance)
        (stream-car s2)
        (stream-limit s2 tolerance))))

;; ex 3.65
; source : http://wqzhang.wordpress.com/2009/08/12/sicp-exercise-3-65/
(define (log2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (log2-summands (+ n 1)))))

(define log2-stream
  (partial-sums (log2-summands 1)))

(define accelerated-log2-stream
  (accelerated-sequence euler-transform log2-stream))

;;; 쌍으로 이루어진 무한 스트림
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs
  (pairs integers integers))

(define custom-pairs 
  (stream-filter (lambda (pair)
                 (prime? (+ (car pair) (cadr pair))))
               int-pairs))


