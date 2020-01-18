#lang sicp

(define (fact-rec n)
  (if (< n 2)
      1
      (* n (fact-rec (- n 1)))))

(define (fact-iter n)
  (define (iter product counter max-count)
    (if (> counter max-count)
        product
        (iter (* product counter)
              (+ counter 1)
              max-count)))
  (iter 1 1 n))


(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

(define (fib-iter n)
  (define (iter p1 p2 n)
    (if (= n 0)
        p1
        (iter p2 (+ p1 p2) (- n 1))))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (iter 0 1 n))))

(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
  (cc amount 5))


;; ex 1.11
(define (ex-1-11-f-rec n)
  (if (< n 3)
      n
      (+ (ex-1-11-f-rec (- n 1))
         (* 2 (ex-1-11-f-rec (- n 2)))
         (* 3 (ex-1-11-f-rec (- n 3))))))

(define (ex-1-11-f-iter n)
  (define (iter a b c count max-count)
    (if (> count max-count)
        c
        (iter b c (+ c (* 2 b) (* 3 a)) (+ count 1) max-count)))
  (if (< n 3)
      n
      (iter 0 1 2 0 (- n 3))))

;; ex 1.12
(define (pascal r c)
  (if (or (= c 1) (= r c))
      1
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))


;; ex 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))

(define (expt1 b n)
  (if (= n 0)
      1
      (* b (expt1 b (- n 1)))))


(define (expt2 base n)
  (define (iter base counter product)
    (if (= counter 0)
        product
        (iter base (- counter 1) (* product base))))
  (iter base n 1))

(define (square x) (* x x))

(define (expt-fast base n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt-fast base (/ n 2))))
        (else (* base (expt-fast base (- n 1))))))

(define (expt-fast-iter base n)
  (define (iter base counter product)
    (cond ((= counter 0) product)
          ((even? counter) (iter (square base) (/ counter 2) product))
          (else (iter base (- counter 1) (* base product)))))
  (iter base n 1))

(expt1 3 9)
(expt2 3 9)
(expt-fast 3 9)
(expt-fast-iter 3 9)

(define (*2 a b)
  (if (= b 0)
      0
      (+ a (*2 a (- b 1)))))

(define (*2d x) (* x 2))
(define (*2h x) (/ x 2))

(define (*2-fast a b)
  (cond ((= b 0) 0)
        ((even? b) (*2d (*2-fast a (*2h b))))
        (else (+ (*2-fast a (- b 1)) a))))

(*2-fast 4 3)

;; ex 1.18
(define (*2-iter a b)
  (define (iter a b sum)
    (cond ((= b 0) sum)
          ((even? b) (iter (*2d a) (*2h b) sum))
          (else (iter a (- b 1) (+ sum a)))))
  (iter a b 0))
(*2-iter 4 9)

;; ex 1.19
(define (s-fib n)
  (s-fib-iter 1 0 0 1 n))

(define (s-fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (s-fib-iter a
                                   b
                                   (+ (square p) (square q))
                                   (+ (* 2 p q) (square q))
                                   (/ count 2)))
        (else (s-fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p q (- count 1)))))

;; 1.2.6
(define (smallest-divisor n)

  (define (divides? a b)
    (= (remainder a b) 0))
  
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(smallest-divisor 49)

(define (prime? n)
  (= (smallest-divisor n) n))

(prime? 113)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
(expmod 9 4 4)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; ex 1.21
(display "ex 1.21\n")
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; ex 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime-fast? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
  

(define (search-for-prime start end)
  (if (< start end)
      (check-and-repeat start end)))

(define (check-and-repeat start end)
  (timed-prime-test start)
  (search-for-prime (+ start 1) end))

;; (search-for-prime 1000 10000)

;; 1.23
(define (smallest-divisor-fast n)

  (define (divides? a b)
    (= (remainder a b) 0))

  (define (next n)
    (if (= n 2) 3 (+ n 2)))
  
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  
  (find-divisor n 2))

(define (prime-fast? n)
  (= (smallest-divisor-fast n) n))

(search-for-prime 10000 100000)