;; (define (sum-int a b)
;;   (if (> a b)
;;       0
;;       (+ a (sum-int (+ a 1) b))))

;; (define (sum-sq a b)
;;   (if (> a b)
;;       0
;;       (+ (square a) (sum-sq (+ a 1) b))))

(define (square a) (* a a))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))

(define (sum-int a b)
  (define (identity x) x)
  (sum identity a inc b))

(define (sum-sq a b)
  (sum square a inc b))


(define (fixed-point f start)
  (define (iter old new)
    (if (close-enough? old new)
        new
        (iter new (f new))))
  (define T 0.00001)
  (define (close-enough? u v) (< (abs (- u v)) T))
  (iter start (f start)))

(define (average x y) (/ (+ x y) 2.0))

(define (sqrt x)
  ;; x/y 의 고정점을 사용하면 진동(oscillate) 할 수 있으므로 average damping 한다
  (define (average-damp f)
    (lambda (x) (average (f x) x)))
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
