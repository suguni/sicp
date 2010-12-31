;; fact(n) = n * (n -1) * (n-2) * ... * 2 * 1
(define (fact1 n)
  (if (= n 1)
      1
      (* n (fact1 (- n 1)))))

;; recursive process
;; (fact1 4)
;; (* 4 (fact 3))
;; (* 4 (* 3 (fact 2)))
;; (* 4 (* 3 (* 2 (fact 1))))
;; (* 4 (* 3 (* 2 1)))
;; (* 4 (* 3 2))
;; (* 4 6)
;; 24

;; iterative process
;; product < counter * product
;; counter < counter - 1
(define (fact2 n)
  (define (iter p c)
    (if (= c 1)
      p
      (iter (* p c) (- c 1))))
  (iter 1 n))

;; (fact2 4)
;; (iter 1 4)
;; (iter 4 3)
;; (iter 12 2)
;; (iter 24 1)
;; 24


;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5)))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9


(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 0 n) = 2 * n
;; (A 1 n) = 2^n
;; (A 2 n) = (((2^2)^2)^2)^ ... n번
;; 2
;; 2^2
;; 16
;; (A 2 n)


;; 0 -> 0
;; 1 -> 1
;; recursive process
(define (fib1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib1 (- n 1)) (fib1 (- n 2))))))

;; fib1 -> n -> pi^n / root(5)

;; f(n) = f(n-1) + f(n-2)
;; iterative process
(define (fib2 n)
  (define (iter a b c)
    (if (= c 0)
        b
        (iter (+ a b) a (- c 1))))
  (iter 1 0 n))
;; (fib 4)
;; (iter 1 0 4)
;; (iter 1 1 3)
;; (iter 2 1 2)
;; (iter 3 2 1)
;; (iter 5 3 0)
;; 3

;; 10
;; 1 5 10 25 50
;; 1 * 10
;; 5 * 2
;; 5 * 1 + 1 * 5 


;; ex 1.11
(define (F1 n)
  (if (< n 3)
      n
      (+ (F1 (- n 1)) (* (F1 (- n 2)) 2) (* (F1 (- n 3)) 3))))

;; a < a + 2*b + 3c
;; b < a 
;; c < b
(define (F2 n)
  (define (iter x y z count)
    (if (< count 3)
        x
        (iter (+ x (* y 2) (* z 3)) x y (- count 1))))
  (iter 2 1 0 n))

;; f(0) = 0
;; f(1) = 1
;; f(2) = 2
;; f(3) = f(2) + 2f(1) + 3f(0) = 4


;; ex 1.12
;; pascal's triangle,
;; p(r, c) = p(r-1, c-1) + p(r-1, c)
;; p(r, 1) = 1, p(r, r) = 1
(define (pt r c)
  (cond ((or (= c 1) (= r c)) 1)
        ((or (< c 0) (< r 0) (> c r)) 0)
        (else (+ (pt (- r 1) (- c 1))
                 (pt (- r 1) c)))))
