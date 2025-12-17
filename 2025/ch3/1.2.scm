(define random-init 1)

(define (rand-update x)
  (+ x 1))

(define (rand)
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trial-passed)
    (cond ((= trials-remaining 0)
           (/ trial-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trial-passed 1)))
          (else
           (iter (- trials-remaining 1) trial-passed))))
  (iter trials 0))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (rand) (random 100))

;; ex 3.5
(define (circle cx cy r)
  (lambda (x y)
    (let ((dx (- x cx))
          (dy (- y cy)))
      (<= (+ (* dx dx) (* dy dy)) (* r r)))))

(define (estimate-integral p x1 y1 x2 y2 trials)
  (define (test) (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (* 1.0 (monte-carlo trials test)) (* (- x2 x1) (- y2 y1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;; ex 3.6 ???
(define (rand m)
  (define x random-init)
  (cond ((eq? m 'generate)
         (begin
           (set! x (rand-update x))
           x))
        ((eq? m 'reset)
         (lambda (new)
           (set! x new)
           x))))
