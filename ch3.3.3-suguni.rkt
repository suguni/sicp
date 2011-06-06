;; 3.3.3 표

(define (c-assoc key records)
  (cond ((null? records) #f)
        ((equal? (car (car records)) key) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (list '*table* ))

;(define (lookup key table)
;  (let ((record (c-assoc key (cdr table))))
;    (if record
;        (cdr record)
;        #f)))
;
;(define (insert! key value table)
;  (let ((record (assoc key (cdr table))))
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
        (let ((record (assoc key-2 (cdr sub-table))))
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
            (let ((record (assoc key-2 (cdr sub-table))))
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

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
