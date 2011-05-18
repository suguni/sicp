;; racket에서는 mutable-pair(set-car!, set-cdr!)를 지원하지 않는다.
;; 언어를 R5RS로 설정해야 함.
;; 참조: http://blog.racket-lang.org/2007/11/getting-rid-of-set-car-and-set-cdr.html

;; 3.3
;; 2장에서 constructor, selector봤고, 여기서는 mutator 알아본다.

;; 3.3.1
(define x '((a b) c d))
(define y '(e f))
(set-car! x y) ;; > ((e f) c d)
(set-cdr! x y) ;; > ((a b) e f)

;; ex 3.12

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(cdr z) ;; '(b c d)

(define w (append! x y))

w ;; '(a b c d)

(cdr x) ;; '(b c d)

;; ex 3.13
(define (make-circle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-circle (list 'a 'b 'c)))
;; (last-pair z) ;; > 무한루프

;; ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
;; w > '(d c b a)
;; v > '(a)

;; page 333. 같이 쓰기와 이름의 실체
;; cons 는 쌍을 만든다.
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
;; z1, z2는 리스트 표현은 동일하지만 box notation으로 보면 틀리다는 것을 알 수 있다. 그림 3.16, 그림 3.17

;; ex3.15
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)
;; (set-to-wow! z1)
;; (set-to-wow! z2)

;; 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; 아래는 잘 되는것 처럼 보이는데..
(count-pairs '(a b c))
(count-pairs '(a (b c)))
(count-pairs '((a) ((b) (c))))
(define c3 (list 'x 'y 'z))
(count-pairs c3) ;; 3

;; 아래는 이상하다.
(define qwer1 (list 'x 'y))
(define qwer2 (cons qwer1 qwer1))
(count-pairs qwer2) ;; 5  
(define asdf1 (list 'x))
(define asdf2 (cons asdf1 asdf1))
(define asdf3 (cons asdf2 asdf2))
(count-pairs asdf3) ;; 7

;; 아래처럼 하면 count-pairs가 끝없이 돌아간다.
;; (set-cdr! (last-pair c3) c3)
;; (count-pairs c3)

;; ex 3.17
;; 위와 같은 문제를 해결한 count-pairs 짜기
(define (count-pairs-new x)  
  (define (checked? list pair)
    (if (null? list)
        #f
        (if (eq? (car list) pair)
            #t
            (checked? (cdr list) pair))))
  
  ;; cols에 확인한 cons를 보관하고 있고, 매번 목록에 있는지 확인
  (let ((cols (list)))
    (define (iter x)
      (if (or (not (pair? x)) (checked? cols x))
          0
          (begin
            (set! cols (cons x cols))
            (+ (iter (car x))
               (iter (cdr x))
               1))))
    (iter x)))

(count-pairs (list 1 2 3)) ;; 3
(count-pairs-new qwer2) ;; 3
(count-pairs-new asdf3) ;; 3
;; ex 3.17 후기: 결과는 맞는데.. 풀이가 구리다!!!
