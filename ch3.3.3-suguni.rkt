;; 3.3.3 표

(define (c-assoc key records)
  (cond ((null? records) #f)
        ((equal? (car (car records)) key) (car records))
        (else (c-assoc key (cdr records)))))

(define (make-table)
  (list '*table* ))

;(define (lookup key table)
;  (let ((record (c-assoc key (cdr table))))
;    (if record
;        (cdr record)
;        #f)))
;
;(define (insert! key value table)
;  (let ((record (c-assoc key (cdr table))))
;    (if record
;        (set-cdr! record value)
;        (set-cdr! table
;                  (cons (cons key value) (cdr table)))))
;  'ok)
;
;(define tab1 (make-table))
;(insert! 'name "steve" tab1)
;(insert! 'id "suguni" tab1)
;(lookup 'name tab1) ;; > steve
;(lookup 'id tab1)   ;; > suguni


;; 위는 1차원 표, 아래는 2차원 표

(define (lookup key-1 key-2 table)
  (let ((sub-table (c-assoc key-1 (cdr table))))
    (if sub-table
        (let ((record (c-assoc key-2 (cdr sub-table))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((sub-table (c-assoc key-1 (cdr table))))
    (if sub-table
        (let ((record (c-assoc key-2 (cdr sub-table))))
          (if record
              (set-cdr! record value)
              (set-cdr! sub-table
                        (cons (cons key-2 value) (cdr sub-table)))))
;        (set-cdr! table
;                  (cons (cons key-1 (list (cons key-2 value)))
;                        (cdr table)))))
        (set-cdr! table
                  (cons (list key-1 (cons key-2 value))
                        (cdr table)))))
  'ok)

(define m-tab (make-table))
(insert! 'math '+ 43 m-tab)
(insert! 'math '- 45 m-tab)
(insert! 'math '* 42 m-tab)
(insert! 'letters 'a 97 m-tab)
(insert! 'letters 'b 98 m-tab)

(lookup 'math '* m-tab)    ;; > 42
(lookup 'letters 'b m-tab) ;; > 98

;; message passing 으로 짜기
(define (make-table-mp)

  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((sub-table (c-assoc key-1 (cdr local-table))))
        (if sub-table
            (let ((record (c-assoc key-2 (cdr sub-table))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((sub-table (c-assoc key-1 (cdr local-table))))
        (if sub-table
            (let ((record (c-assoc key-2 (cdr sub-table))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! sub-table
                            (cons (cons key-2 value) (cdr sub-table)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
  
    dispatch))

;(define operation-table (make-table-mp))
;(define get (operation-table 'lookup-proc))
;(define put (operation-table 'insert-proc!))

;; ex 3.24
;; 그냥 assoc 을 내부에 숨기고, assoc이 equal? 대신 인자로 받은 same-key?를 사용하도록 수정.
;; 이렇게 하는건 너무 간단하자나!!! 다른 방법이 있나???
(define (make-table-sk same-key?)

  (define (mt-assoc key records)
    (cond ((null? records) #f)
          ((same-key? (car (car records)) key) (car records))
          (else (mt-assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((sub-table (mt-assoc key-1 (cdr local-table))))
        (if sub-table
            (let ((record (mt-assoc key-2 (cdr sub-table))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((sub-table (mt-assoc key-1 (cdr local-table))))
        (if sub-table
            (let ((record (mt-assoc key-2 (cdr sub-table))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! sub-table
                            (cons (cons key-2 value) (cdr sub-table)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
  
    dispatch))

;(define operation-table (make-table-sk equal?))
;(define get (operation-table 'lookup-proc))
;(define put (operation-table 'insert-proc!))
;
;(put 'math 'plus +)
;(put 'math 'minus -)
;((get 'math 'plus) 1 2 3)


;; 3.25 아래처럼 여러개의 열쇠가 가능하도록 하고 싶다.
;; (define my-table (make-table))
;; (insert! '(k1 k2 k3 k4) "v1" my-table)
;; (lookup '(k1 k2 k3 k4) my-table)

(define (make-table-mp)

  (let ((local-table (list '*table*)))
    
    (define (lookup keys)
      (define (iter ks table)
        (if (or (null? ks) (null? table))
            #f
            (let ((record (c-assoc (car ks) (cdr table))))
              (if record
                  (if (pair? (cdr record))
                      (iter (cdr ks) record)
                      (cdr record))
                  #f))))
      (iter keys local-table))

    (define (insert! keys value)

      (define (make-sub-table ks)
        (if (null? (cdr ks))
            (cons (car ks) value)
            (list (car ks) (make-sub-table (cdr ks)))))

      (define (iter! ks table)
        (if (null? ks)
            (error "NULL error" ks)
            (if (pair? (cdr table))
                (let ((record (c-assoc (car ks) (cdr table))))
                  (if record
                      (if (null? (cdr ks))
                          (set-cdr! record value)
                          (iter! (cdr ks) record))
                      (set-cdr! table
                                (cons (make-sub-table ks) (cdr table)))))
                (set-cdr! table
                          (cons (make-sub-table ks) '())))))
      (iter! keys local-table)
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'table) local-table)
            (else (error "Unknown operation -- TABLE" m))))
  
    dispatch))

(define tab1 (make-table-mp))
(define get (tab1 'lookup-proc))
(define put (tab1 'insert-proc!))

(put '(a a a) 1)
(put '(a a b) 2)
(put '(a a c) 3)
(put '(a b a) 4)
(put '(a b b) 5)
(put '(a b c) 6)
(put '(a c a) 7)
(put '(a c b) 8)
(put '(a c c) 9)

(get '(a a c)) ;; >> 3

;; 아래처럼 입력하고
(put '(a a a b b b c c c) 12345)
;; 아래는?
(get '(a a a))
;; 이상하다.
