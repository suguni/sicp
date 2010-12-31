#lang racket

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder a b) 0))
  
  (define (square x)
    (* x x))
  
  ;; + 1
  (define (find-divisor value test-div)
    (cond ((divides? value test-div) test-div)
          ((> (square test-div) value) value)
          (else (find-divisor value (+ test-div 1)))))
  
  ;; next
  (define (next n)
    (if (= n 2) 3 (+ n 2)))
  
  (define (find-divisor-2 value test-div)
    (cond ((divides? value test-div) test-div)
          ((> (square test-div) value) value)
          (else (find-divisor-2 value (next test-div)))))

;;  (find-divisor n 2)
  (find-divisor-2 n 2)
  )

(define (square x)
  (* x x))

(define (prime?-2 n times)
  (cond ((= times 0) #t) ;; fermat 테스트를 times 횟수 반복했는데 모두 #t 인 경우
        ((fermat-test n) (prime?-2 n (- times 1))) ;; fermat 테스트를 통과한 경우
        (else #f))) ;; fermat 테스트를 통과하지 못한 경우

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ (random (- n 1)) 1)))



;; 1.22
(define (timed-prime-test n)
;  (display n)
;  (newline)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n) ; 100)
      (report-prime n (- (current-inexact-milliseconds) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-prime n c)
  (if (= c 0)
      (newline)
      (search-for-prime
       (+ n 1)
       (if (timed-prime-test n)
           (- c 1)
           c))))


;(search-for-prime 1000 3)
;(search-for-prime 10000 3)
;(search-for-prime 100000 3)
;(search-for-prime 1000000 3)
;(search-for-prime 10000000 3)
;(search-for-prime 100000000 3)
;(search-for-prime 1000000000 3)
;(search-for-prime 10000000000 3)
;(search-for-prime 100000000000 3)
;(search-for-prime 1000000000000 3)
;(search-for-prime 10000000000000 3)



(define (prime-by-fermat-all-number? n)
  (define (iter a)
    (if (< a 2) #t
        (if (= (expmod a n n) a) (iter (- a 1)) #f)))
  (iter (- n 1)))
 
(define (camichael-number? n)
  (and (not (prime? n)) (prime-by-fermat-all-number? n)))




(define (prime-by-miller-test? n times)
  (cond ((= times 0) #t)
        ((miller-test n) (prime-by-miller-test? n (- times 1)))
        (else #f)))

;; ???
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ (random (- n 1)) 1)))
