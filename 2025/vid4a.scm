(define (atom? v) (not (pair? v)))

(define derive-rules
  '(
    ( (dd (?c c) (? v)) 0 )
    ( (dd (?v v) (? v)) 1 )
    ( (dd (?v u) (? v)) 0 )

    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
        (dd (: x2) (: v))))

    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1) (dd (: x2) (: v)))
        (* (dd (: x1) (: v)) (: x2))))

    ((dd (** (? x) (?c n)) (? v))
     (* (* (: n) (** (: x) (: (- n 1))))
        (dd (: x) (: v))))

    ))

;; (? x1) pattern variable
;; (: x1) substitution object


;; pattern match
;; x
;; (f a b)
;; (: x) x
;; (:c x) constant
;; (:v x) variable


;; skeleton
;; foo     -> foo
;; (f a b) -> '(foo a b)
;; (: x)   -> value of x
;;
;;

(define compound? pair?)

(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp)
                   exp)))

  (define (simplify-parts exp)
    (if (null? exp)
        '()
        (cons (simplify-exp (car exp))
              (simplify-parts (cdr exp)))))

  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dict (match (pattern (car rules))
                        exp
                        (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-exp (instantiate (skeleton (car rules)) dict))))))
    (scan the-rules))
  simplify-exp)

(define dd-simp (simplifier derive-rules))

(dd-simp '(dd (+ x y)) 'x)
;; => (+ 1 0)

;; matched
;; pat  (+ (* (? x) (? y)) (? y))
;; exp  (+ (*    3     x)     x )
;;
;; failed
;; pat  (+ (* (? x) (? y)) (? y))
;; exp  (+ (*    3     x)     3 )

(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)

        ((atom? pat)
         (if (and (atom? exp)
                  (eq? pat exp))
             dict
             'failed))

        ;; (?c e)
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dict pat exp dict)
             'failed))

        ;; (?v e)
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dict pat exp dict)
             'failed))

        ;; (? e)
        ((arbitrary-expression? pat)
         (extend-dict pat exp dict))

        ((atom? exp) 'failed)
        (else
         (match (cdr pat)
           (cdr exp)
           (match (car pat)
             (car exp)
             dict)))))

;; skeleton  (+ (dd (: x1) (: v)) (dd (: x2) (: v)))
;; dict      ((x1 10) (x2 2) (v 2))

(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
             user-initial-environment)
       (mapcar (lambda (v) (lookup v dict))
               (cdr form)))))

(define user-initial-environment '())
