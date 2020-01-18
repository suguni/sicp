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

