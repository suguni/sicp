;; ex 3.9
;; recursive factorial
;;
;; global env -> factorial
;; (factorial 3)
;; e1 -> n=3
;; (factorial n)
;; 
;; e2 -> n=2
;; (factorial n)
;; 
;; e3 -> n=1
;; (factorial n)
;;
;; return 6
;;
;; iterative factorial
;; gloabl env -> fatorial, fact-iter
;; (factorial 3)
;; e1 -> n=3
;; (fact-iter 1 1 n)
;; 
;; e2 -> product=1 counter=1 max-count=3
;; (if ... )
;;
;; e3 -> product=1 counter=2 max-count=3
;; (if ... )
;;
;; e4 -> product=2 counter=3 max-count=3
;; (if ... )
;;
;; e5 -> product=6 counter=4 max-count=3
;; (if ... )
;;
;; return 6

;; ex 3.13
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; ex 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs '(1 2 3)) ;; 3

(let* ((z (cons '3 '()))
       (y (cons '2 z))
       (x (cons y z)))
  (count-pairs x)) ;; 4

(let* ((z (cons 1 2))
       (y (cons z z))
       (x (cons y y)))
  (count-pairs x))


;; ex 3.17
(define (contains? xs x)
  (if (null? xs)
      #f 
      (if (eq? (car xs) x)
          x
          (contains? (cdr xs) x))))

(define (count-pairs x)
  (define visited '())
  (define (iter p)
    (if (or (not (pair? p))
            (contains? visited p))
        0
        (begin
          (set! visited (cons p visited))
          (+ (iter (car p))
             (iter (cdr p))
             1))))
  (iter x))

(count-pairs '(1 2 3)) ;; 3

(let* ((z (cons '3 '()))
       (y (cons '2 z))
       (x (cons y z)))
  (count-pairs x)) ;; 3

(let* ((z (cons 1 2))
       (y (cons z z))
       (x (cons y y)))
  (count-pairs x)) ;; 3

;; ex 3.18
(define (cyclic? x)
  (define (iter n visited)
    (cond
     ((not (pair? n)) #f)
     ((null? (cdr n)) #f)
     ((contains? visited n) #t)
     (else
      (iter (cdr n) (cons n visited)))))
  (iter x '()))

(cyclic? '(1 2)) ;; #f

(let ((a (make-cycle (list 'a 'b 'c))))
  (cyclic? a)) ;; #t

(let* ((a (make-cycle (list 'a 'b 'c)))
       (z (cons 'z a)))
  (cyclic? z)) ;; #t

(let* ((b (cons 1 '()))
       (a (cons b 2)))
  (set-cdr! b a)
  (cyclic? a)) ;; #f

;; ex 3.19
;; 정해진 공간만 쓴다는 것이 어떤 의미? 말 그대로
;; Floyd’s Cycle Detection Algorithm
(define (cyclic? x)
  (define (iter n1 n2)
    (cond
     ((eq? n1 n2) #t)
     ((or (not (pair? n1)) (not (pair? n2)) (not (pair? (cdr n2)))) #f)
     ((or (null? (cdr n1)) (null? (cdr n2))) #f)
     (else
      (iter (cdr n1) (cdr (cdr n2))))))
  (if (not (pair? x)) #f (iter x (cdr x))))


;; ch 3.3.2

(define (make-queue)
  (cons '() '()))

(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called with an empty queue" q)
      (car (front-ptr q))))

(define (insert-queue! q v)
  (let ((new-pair (cons v '())))
    (if (empty-queue? q)
        (set-car! q new-pair)
        (set-cdr! (rear-ptr q) new-pair))
    (set-cdr! q new-pair)
    q))

(define (delete-queue! q)
  (if (empty-queue? q)
      (error "DELETE called with an empty queue" q)
      (set-car! q (cdr (front-ptr q))))
  q)


;; ex 3.21
;; rear-ptr 은 여전히 마지막에 insert 한 pair 를 가리킨다.
(define (print-queue q)
  (define (print-iter n)
    (if (null? n)
        '()
        (begin
          (display (car n))
          (if (not (null? (cdr n))) (display " ") '())
          (print-iter (cdr n)))))
  (display "(")
  (print-iter (car q))
  (display ")\n")
  )

(define q (let ((q (make-queue)))
            (insert-queue! q 'a)
            (insert-queue! q 'b)
            (insert-queue! q 'c)
            q))


(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty?)
      (null? front-ptr))

    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue" q)
          (car front-ptr)))

    (define (insert! v)
      (let ((new-pair (cons v '())))
        (if (empty?)
            (set! front-ptr new-pair)
            (set-cdr! rear-ptr new-pair))
        (set! rear-ptr new-pair)))

    (define (delete!)
      (if (empty?)
          (error "DELETE called with an empty queue" q)
          (set! front-ptr (cdr front-ptr))))
    
    (define (print)
      (define (print-iter n)
        (if (null? n)
            '()
            (begin
              (display (car n))
              (if (not (null? (cdr n))) (display " ") '())
              (print-iter (cdr n)))))
      (display "(")
      (print-iter front-ptr)
      (display ")\n"))

    (define (dispatch m)
      (cond
       ((eq? m 'empty?) empty?)
       ((eq? m 'front) front)
       ((eq? m 'insert!) insert!)
       ((eq? m 'delete!) delete!)
       ((eq? m 'print) print)
       (else (error "INVALID message" m))))
    
    dispatch))

(define (empty-queue? q)
  ((q 'empty?)))

(define (front-queue q)
  ((q 'front)))

(define (insert-queue! q v)
  ((q 'insert!) v))

(define (delete-queue! q)
  ((q 'delete!)))

(define (print-queue q)
  ((q 'print)))


;; ex 3.23

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (front-node v)
      (cons (cons v '()) front-ptr))

    (define (rear-node v)
      (cons (cons v rear-ptr) '()))
    
    (define (empty?)
      (or (null? front-ptr) (null? rear-ptr)))

    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue")
          (car (car front-ptr))))

    (define (rear)
      (if (empty?)
          (error "REAR called with an empty queue")
          (car (car rear-ptr))))

    (define (front-insert! v)
      (let ((new-node (front-node v)))
        (if (empty?)
            (set! rear-ptr new-node)
            (set-cdr! (car front-ptr) new-node))
        (set! front-ptr new-node)))
    
    (define (rear-insert! v)
      (let ((new-node (rear-node v)))
        (if (empty?)
            (set! front-ptr new-node)
            (set-cdr! rear-ptr new-node))
        (set! rear-ptr new-node)))

    (define (front-delete!)
      (if (empty?)
          (error "FRONT-DELETE called with an empty queue")
          (begin
            (set! front-ptr (cdr front-ptr))
            (if (not (null? front-ptr))
                (set-cdr! (car front-ptr) '())
                '()))))
    
    (define (rear-delete!)
      (if (empty?)
          (error "REAR-DELETE called with an empty queue")
          (begin
            (set! rear-ptr (cdr (car rear-ptr)))
            (if (not (null? rear-ptr))
                (set-cdr! rear-ptr '())
                '()))))

    (define (print)
      (define (iter node)
        (display (car (car node)))
        (if (null? (cdr node))
            '()
            (begin
              (display " ")
              (iter (cdr node)))))
      (display "(")
      (if (empty?)
          '()
          (iter front-ptr))
      (display ")\n"))

    (define (dispatch m)
      (cond
       ((eq? m 'empty?) empty?)
       ((eq? m 'front) front)
       ((eq? m 'rear) rear)
       ((eq? m 'front-insert!) front-insert!)
       ((eq? m 'rear-insert!) rear-insert!)
       ((eq? m 'front-delete!) front-delete!)
       ((eq? m 'rear-delete!) rear-delete!)
       ((eq? m 'print) print)
       ((eq? m 'front-ptr) front-ptr)
       ((eq? m 'rear-ptr) rear-ptr)
       (else (error "INVALID message" m))))
    
    dispatch))

(define (empty-deque? q)
  ((q 'empty?)))

(define (front-deque q)
  ((q 'front)))

(define (rear-deque q)
  ((q 'rear)))

(define (front-insert-deque! q v)
  ((q 'front-insert!) v))

(define (front-delete-deque! q)
  ((q 'front-delete!)))

(define (rear-insert-deque! q v)
  ((q 'rear-insert!) v))

(define (rear-delete-deque! q)
  ((q 'rear-delete!)))

(define (print-deque q)
  ((q 'print)))
