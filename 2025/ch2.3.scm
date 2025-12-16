(define (atom? exp)
  (not (pair? exp)))

(define (derive exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp)
         (make-sum (derive (a1 exp) var)
                   (derive (a2 exp) var)))
        ((product? exp)
         (make-sum
          (make-product (derive (m1 exp) var) (m2 exp))
          (make-product (m1 exp) (derive (m2 exp) var))))))

(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))

(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

(define (sum? exp) (and (not (atom? exp)) (eq? (car exp) '+)))
;; (define (make-sum a1 a2) (list '+ a1 a2))
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((eq? a1 0) a2)
        ((eq? a2 0) a1)
        (#t (list '+ a1 a2))))
(define (a1 s) (car (cdr s)))
(define (a2 s) (car (cdr (cdr s))))

(define (product? exp) (and (not (atom? exp)) (eq? (car exp) '*)))
;; (define (make-product m1 m2) (list '* m1 m2))
(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((or (eq? m1 0) (eq? m2 0)) 0)
        ((eq? m1 1) m2)
        ((eq? m2 1) m1)
        (#t (list '* m1 m2))))
(define (m1 s) (car (cdr s)))
(define (m2 s) (car (cdr (cdr s))))

(define foo
  '(+
    (* a (* x x))
    (+ (* b x)
       c)))
