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

(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;; ex 3.53
(define s (cons-stream 1 (add-stream s s)))

;; ex 3.54
(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-stream factorials integers)))

;; ex 3.55
(define (partial-sums s)
  (define iter
    (cons-stream (stream-car s)
                 (add-stream iter
                             (stream-cdr s))))
  iter)
