
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

;; ex 3.22
(define (print-queue queue)
  (write (car queue)))
