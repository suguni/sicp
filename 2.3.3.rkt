#lang racket

;; element-of-set?, adjoin-set, union-set, intersection-set 프로시저가 있을때 아래가 성립한다.
;; (element-of-set? x (adjoin-set x S))                 ;; 항상 참
;; (= (element-of-set? x (union-set S T)) 
;;    (or (element-of-set? x S) (element-of-set? x T))) ;; 항상 참
;; (element-of-set? x '())                              ;; 항상 거짓

;; unordered list
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x (car set1)))
        (if (element-of-set? x set2)
            (adjoin-set x (intersection-set (cdr set1) set2))
            (intersection-set (cdr set1) set2)))))

;; ex 2.59
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (let ((x (car set1))
            (sub-set (cdr set1)))
        (if (element-of-set? x set2)
            (union-set sub-set set2)
            (adjoin-set x (union-set sub-set set2))))))

;; ex 2.60 - 중복이 허용하게 만들 경우
;; 동일
(define (element-of-set-d? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
;; 변경
(define (adjoin-set-d x set)
  (cons x set))
;; 동일
(define (intersection-set-d set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x (car set1))
            (sub-set (cdr set1)))
        (if (element-of-set-d? x set2)
            (adjoin-set-d x (intersection-set-d sub-set set2))
            (intersection-set-d sub-set set2)))))
;; 변경
(define (union-set-d set1 set2)
  (if (null? set1)
      set2
      (adjoin-set-d (car set1) (union-set-d (cdr set1) set2))))
;; 효율 - 중복 허용이 효율이 높다. 
;;        adjoin-set의 계산은 element-of-set? 이 호출되지 않으므로 OMEGA(1)
;;        intersection-set 에서는 element-of-set?을 호출하므로 OMEGA(n) XXX -> OMEGA(n^2)
;;        union-set 에서는 호출되지 않으므로 OMEGA(1) ??? XXX -> OMEAGA(n)
;; 더 잘 맞아떨어지는 문제? - 공간 효율성이 문제가 되지 않는 경우. 실 사례???

;; ordered-list 인 경우
(define (element-of-set-ol? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set-ol? x (cdr set)))))

(define (intersection-set-ol set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2))
            (sub-set1 (cdr set1))
            (sub-set2 (cdr set2)))
        (cond ((equal? x1 x2) (adjoin-set x1 (intersection-set-ol sub-set1 sub-set2)))
              ((> x1 x2) (intersection-set-ol set1 sub-set2))
              ((< x1 x2) (intersection-set-ol sub-set1 set2))))))

;; ex 2.61
(define (adjoin-set-ol x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
;;        ((> x (car set)) (cons (car set) (adjoin-set-ol x (cdr set))))))
        (else (cons (car set) (adjoin-set-ol x (cdr set))))))
;; 차례 매김 표현 방식의 좋은 점이 어떻게 도움이 되는가?
;;   모든 원소를 확인할 필요가 없다. element-of-set을 호출할 필요가 없다.

;; ex 2.62
(define (union-set-ol set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2))
               (sub-set1 (cdr set1))
               (sub-set2 (cdr set2)))
           (cond ((equal? x1 x2) (adjoin-set-ol x1 (union-set-ol sub-set1 sub-set2)))
                 ((> x1 x2)      (adjoin-set-ol x2 (union-set-ol set1 sub-set2)))
                 ((< x1 x2)      (adjoin-set-ol x1 (union-set-ol sub-set1 set2))))))))
;; 이게 OMEGA(n)인가? OK
;; adjoin-set-ol 필요 없음. cons로 엮어도 됨.

;; 두 갈래 나무로 표현한 집합

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-tree? x set)
  (cond ((null? set) #f)
        ((= (entry set) x) #t)
        ((> (entry set) x) (element-of-set-tree? x (left-branch set)))
        ((< (entry set) x) (element-of-set-tree? x (right-branch set)))))

(define set-tree (make-tree 8
                         (make-tree 4 (make-tree 2 '() '()) (make-tree 6 '() '()))
                         (make-tree 12 (make-tree 10  '() '()) (make-tree 14 '() '()))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (entry set) x) set)
        ((> (entry set) x) (make-tree (entry set)
                                      (adjoin-set-tree x (left-branch set))
                                      (right-branch set)))
        ((< (entry set) x) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set-tree x (right-branch set))))))

;; fig 2.17
(define (make-skewed-set-tree n)
  (define (iter i tree)
    (if (> i n)
        tree
        (iter (+ i 1) (adjoin-set-tree i tree))))
  (iter 1 '()))
;; (make-skewed-set-tree 7)

;; ex 2.63
;; tree->list-1
;(TI-1 '(2 (1 () ()) (3 () ())))
;(append (TI-1 '(1 () ()))
;        (cons 2 (TI-1 '(3 () ()))))
;(append
; (append (TI-1 '())
;         (cons 1 (TI-1 '())))
; (cons 2 (append (TI-1 '())
;                 (cons 3 (TI-1 '())))))
;(append
; (append '() '(1))
; (cons 2 (append '() (3))))
;(append '(1) (cons 2 '(3)))
;(append '(1) '(2 3))
;'(1 2 3)

;; tree->list-2
;(TI-2 '(2 (1 () ()) (3 () ())))
;(iter '(2 (1 () ()) (3 () ())) '())
;(iter '(1 () ())
;      (cons 2 (iter '(3 () ()) '())))
;(iter '(1 () ())
;      (cons 2 (iter '()
;                    (cons 3 (iter '() '())))))
;(iter '(1 () ())
;      (cons 2 (iter '() '(3))))
;(iter '(1 () ())
;      (cons 2 '(3)))
;(iter '(1 () ()) '(2 3))
;(iter '()
;      (cons 1 (iter '() '(2 3))))
;(iter '()
;      (cons 1 '(2 3)))
;(iter '() '(1 2 3))
;'(1 2 3)

;; 위는 (2 (1 () ()) (3 () ()))에 대해 직접 손으로 풀어본 것.

;; a. 둘다 같은 결과를 내놓는다.
;; fig 2.16 (아래 set들)에 적용하면 (1 3 5 7 9 11)가 된다.
(define set-2-16-a (make-tree 7
                              (make-tree 3
                                         (make-tree 1 '() '())
                                         (make-tree 5 '() '()))
                              (make-tree 9
                                         '()
                                         (make-tree 11 '() '()))))
(define set-2-16-b (make-tree 3
                              (make-tree 1 '() '())
                              (make-tree 7
                                         (make-tree 5 '() '())
                                         (make-tree 9
                                                    '()
                                                    (make-tree 11 '() '())))))
(define set-2-16-c (make-tree 5
                              (make-tree 3
                                         (make-tree 1 '() '())
                                         '())
                              (make-tree 9
                                         (make-tree 7 '() '())
                                         (make-tree 11 '() '()))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (iter t result)
    (if (null? t)
        result
        (iter (left-branch t)
              (cons (entry t)
                    (iter (right-branch t) result)))))
  (iter tree '()))

;; b. no idea
;; 동일한 balanced tree로 1, 2를 실행했을때 동일한 자람차수를 가지는지? => NO
;; 느린쪽은 어디인지?
;;   1번 아이디어. tree->list-1, append가 n의 자람차수이므로 전체적으로 n^2 이다. 
;;   2번 아이디어. tree->list-2, 

;; 2.64
;; ordered list를 balanced binary tree로 바꾸는 프로시저
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; a.
;; partial-tree 프로시저의 동작 방법
;;  1. n을 이용해 왼쪽 tree의 개수를 확인하여, elts에 대해 왼쪽 개수만큼의 tree를 생성한다.
;;  2. 1에서 나온 결과에서 tree로 만들어지지 않은 리스트에서 첫번째 원소는 entry가 된다.
;;  3. 1에서 나온 결과에서 tree로 만들어지지 않은 리스트에서 첫번째를 제외한 나머지를 입력으로 하고
;;     왼쪽 개수 + 1을 오른쪽 개수로 하여 tree를 생성한다.
;;  4. 1, 2, 3의 결과로 make-tree하여 tree를 만들어 낸다.
;; (1 3 5 7 9 11)을 넣었을때 결과는? (list->tree '(1 3 5 7 9 11)) 해보면 된다.
;; b. 원소가 n개인 리스트를 바꾸는데 드는 계산 단계?
;;  n? log n?

;; ex 2.65
;; tree->list, list-tree, intersection-set-ol, union-set-ol 모두가 OMEGA(n)이므로 아래도 OMEGA(n)이다.
(define (union-set-tree set1 set2)
  (let ((list1 (tree->list-1 set1))
        (list2 (tree->list-1 set2)))
    (let ((u (union-set-ol list1 list2)))
      (list->tree u))))
(define (intersection-set-tree set1 set2)
  (let ((list1 (tree->list-1 set1))
        (list2 (tree->list-1 set2)))
    (let ((u (intersection-set-ol list1 list2)))
      (list->tree u))))
(define s1 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())))
(define s2 (make-tree 5 (make-tree 3 '() '()) (make-tree 7 '() '())))
;;   사실, 63에서 tree->list-1과 tree->list-2의 자람차수를 풀지 못했고,
;;   64에서도 자람차수를 풀지 못해서.. 정확하지 않음.
;;   하지만 정황상 63, 64 모두 최소 OMEGA(n) 이하 일테니 아래 성립함. ㅋㅋ
;;   http://eli.thegreenplace.net/2007/09/11/sicp-section-233/ 에서도 위와 동일하게 풀었음. ^^

;; p207 집합에서 정보 찾아내기

;; ex 2.66
;; key 프로시저가 없으므로 확인 불가하지만 대충 맞는듯. 사실 앞에서 본 element-of-set-tree?와 동일하다.
(define (lookup-tree? given-key set-of-records)
  (cond ((null? set-of-records)
         #f)
        ((= (key (entry set-of-records)) given-key)
         #t)
        ((> (key (entry set-of-records)) given-key)
         (lookup-tree? given-key (left-branch set-of-records)))
        ((> (key (entry set-of-records)) given-key)
         (lookup-tree? given-key (right-branch set-of-records)))))

