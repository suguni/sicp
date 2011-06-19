
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

(define (insert-queue-new! queue item)
  ((queue 'insert!) item))
(define (delete-queue-new! queue)
  ((queue 'delete!)))
(define (empty-queue-new? queue)
  ((queue 'empty?)))
(define (front-queue-new queue)
  ((queue 'front)))
(define (print-queue-new queue)
  ((queue 'print)))

;(define nq (make-queue-new))
;(insert-queue-new! nq 'a) ;; (a)
;(print-queue-new nq)
;(insert-queue-new! nq 'b) ;; (a b)
;(print-queue-new nq)
;(insert-queue-new! nq 'c) ;; (a b c)
;(print-queue-new nq)
;(insert-queue-new! nq 'd) ;; (a b c d)
;(print-queue-new nq)
;(empty-queue-new? nq)     ;; false
;(front-queue-new nq)      ;; a
;(delete-queue-new! nq)    ;; (b c d)
;(delete-queue-new! nq)    ;; (c d)
;(front-queue-new nq)      ;; c
;(print-queue-new nq)

;; ex 3.23 double-ended queue 만들기
;; 모든 연산이 O(1) 이어야 한다.
;; > 이것때문에 위의 queue 구조로 풀 수 없다.
;; > rear-delete-deque를 O(1)로 만들수 없기 때문.
;; > 데이터와 이전 pair를 쌍으로 하여 데이터 구조를 만든다.
(define (make-deque)
  (cons '() '()))

(define (empty-deque? deque)
  (or (null? (front-deque-ptr deque))
      (null? (rear-deque-ptr deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque) ;; error
      (car (car (front-deque-ptr deque)))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque) ;; error
      (car (car (rear-deque-ptr deque)))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
           (set-front-deque-ptr! deque new-pair)
           (set-rear-deque-ptr! deque new-pair))
          (else
           (set-cdr! new-pair (front-deque-ptr deque))
           (set-cdr! (car (front-deque-ptr deque)) new-pair)
           (set-front-deque-ptr! deque new-pair)))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
           (set-front-deque-ptr! deque new-pair)
           (set-rear-deque-ptr! deque new-pair))
          (else
           (set-cdr! (car new-pair) (rear-deque-ptr deque))
           (set-cdr! (rear-deque-ptr deque) new-pair)
           (set-rear-deque-ptr! deque new-pair)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque) ;; 빈 경우
         (error "DELETE! called with an empty deque" deque))
        ((null? (cdr (front-deque-ptr deque))) ;; 1개 남은 경우
         (set-front-deque-ptr! deque '())
         (set-rear-deque-ptr! deque '()))
        (else
         (set-cdr! (car (cdr (front-deque-ptr deque))) '())
         (set-front-deque-ptr! deque (cdr (front-deque-ptr deque))))))
  
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque) ;; 빈 경우
         (error "DELETE! called with an empty deque" deque))
        ((null? (cdr (front-deque-ptr deque))) ;; 1개 남은 경우
         (set-front-deque-ptr! deque '())
         (set-rear-deque-ptr! deque '()))
        (else
         (set-rear-deque-ptr! deque (cdr (car (rear-deque-ptr deque))))
         (set-cdr! (rear-deque-ptr deque) '()))))

(define (front-deque-ptr deque) (car deque))
(define (rear-deque-ptr deque) (cdr deque))
(define (set-front-deque-ptr! deque item) (set-car! deque item))
(define (set-rear-deque-ptr! deque item) (set-cdr! deque item))

(define (print-deque deque)
  (define (iter curr)
    (if (eq? (rear-deque-ptr deque) curr)
        (begin
          (write (car (car curr))))
        (begin
          (write (car (car curr)))
          (iter (cdr curr)))))
  (iter (front-deque-ptr deque)))

;; deque 테스트
;(define de-queue (make-deque))
;(empty-deque? de-queue) ;; #t
;
;(rear-insert-deque! de-queue 'a)
;(print-deque de-queue) ;; a
;(newline)
;(rear-insert-deque! de-queue 'b)
;(print-deque de-queue) ;; ab
;(newline)
;(front-insert-deque! de-queue 'c)
;(print-deque de-queue) ;; cab
;(newline)
;(front-insert-deque! de-queue 'd)
;(print-deque de-queue) ;; dcab
;(newline)
;(front-delete-deque! de-queue)
;(print-deque de-queue) ;; cab
;(newline)
