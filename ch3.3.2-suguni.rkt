
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


;; 3.3.2 큐

;; (define q (make-queue))
;; (insert-queue! q 'a) ;; (a)
;; (insert-queue! q 'b) ;; (a b)
;; (delete-queue! q)    ;; b
;; (insert-queue! q 'c) ;; b c
;; (empty-queue? q)     ;; false
;; (front-queue q)      ;; b

;; 아래처럼 만들면 안된다!!!
;(define (make-queue)
;  (list))
;(define (insert-queue! l v)
;  (begin
;    (if (null? l)
;        (set! l (list v))
;        (append! l (list v)))
;    l))
;(define (delete-queue! l)
;  (begin
;    (set-car! l (cdr q))
;    l))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue) ;; error
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; ex 3.21
;; queue는 front-ptr과 rear-ptr의 cons 쌍이므로,
;; 논리적인 큐의 형태만 출력하려면 car만 출력하면 된다.
;; 여기서는 그냥 car를 반환하게 해서 repl이 알아서 출력하게 함.
(define (print-queue queue)
  (write (car queue)))

;; ex 3.22
(define (make-queue-new)
  (let ((front-ptr '())
        (rear-ptr '()))
    
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))

    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue") ;; error
          (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set-front-ptr! (cdr front-ptr)))))
    
    (define (print-queue)
      front-ptr)
    
    (define (dispatch m)
      (cond ((eq? m 'insert!) insert-queue!)
            ((eq? m 'delete!) delete-queue!)
            ((eq? m 'front) front-queue)
            ((eq? m 'empty?) empty-queue?)
            ((eq? m 'print) print-queue)
            (else
             (error "Invalid Message"))))
    
    dispatch))

(define (insert-queue! queue item)
  ((queue 'insert!) item))
(define (delete-queue! queue)
  ((queue 'delete!)))
(define (empty-queue? queue)
  ((queue 'empty?)))
(define (front-queue queue)
  ((queue 'front)))
(define (print-queue queue)
  ((queue 'print)))

(define nq (make-queue-new))
(insert-queue! nq 'a) ;; (a)
(print-queue nq)
(insert-queue! nq 'b) ;; (a b)
(print-queue nq)
(insert-queue! nq 'c) ;; (a b c)
(print-queue nq)
(insert-queue! nq 'd) ;; (a b c d)
(print-queue nq)
(empty-queue? nq)     ;; false
(front-queue nq)      ;; a
(delete-queue! nq)    ;; (b c d)
(delete-queue! nq)    ;; (c d)
(front-queue nq)      ;; c
(print-queue nq)

;; ex 3.23
