(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment s)
  (let* ((start (start-segment s))
         (end (end-segment s))
         (sx (x-point start))
         (sy (y-point start))
         (ex (x-point end))
         (ey (y-point end)))
    (make-point (+ sx (/ (- ex sx) 2))
                (+ sy (/ (- ey sy) 2)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (ex2-2)
  (print-point
   (midpoint-segment
    (make-segment (make-point 0 0)
                  (make-point 2 2)))))

(define (mcons x y)
  (define (dispatch m)
    (cond
     ((= m 0) x)
     ((= m 1) y)
     (else (error "Argument not 0 or 1 -- MCONS" m))))
  dispatch)
(define (mcar z) (z 0))
(define (mcdr z) (z 1))

(define (display-mcons z)
  (display (format #t "(~a . ~a)\n" (mcar z) (mcdr z)))
  '())

(mcar (mcons 1 2))
