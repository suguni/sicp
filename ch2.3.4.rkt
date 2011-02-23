#lang racket

;; ch 2.3.4

;; leaf node constructor/selector
;; ('leaf symbol weight)
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; huffman tree constructor/selector
;; (left-branch right-branch symbols weight)
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; 정리
;;   - 허프만 인코딩은 허프만 나무를 이용해, 입력 데이터를 길이가 변하는(variable length) 코드로 변경하는 방법이다.
;;   - 허프만 나무는 두 갈래 나무꼴(binary tree)이며, 잎(leaf node)에는 인코딩할 글자와 무게(글속에 나온 횟수)를
;;     잎이 아닌 마디에는 해당 마디 아래 달려있는 모든 잎을 원소로 하는 글자 집합과 각 잎의 무게의 합을 가진다.
;;   허프만 나무를 만드는 방법
;;     원리: 가장 적게 나오는 글자가 나무 뿌리(root)에서 가장 멀게 만든다.
;;     1. 문자와 무게(출현 빈도)를 쌍으로 한 나뭇잎 집합을 만든다. - 데이터를 이용하여 정한다.
;;     2. 나뭇잎 집합에서 가장 가벼운 두 개 나뭇잎을 왼쪽/오른쪽 가지로 하는 새로운 마디를 만든다.
;;        이 마디의 무게는 두 나뭇잎 무게의 합이다.
;;     3. 1,2단계를 반복한다.
;;   인코딩하기
;;     데이터에서 주어진 문자마다 허프만 나무에서의 해당 문자 값을 반환
;;     허프만 나무에서의 문자값은 나무 뿌리에서 왼쪽 가지면 코드 0을, 오른쪽 가지면 코드 1을 비트로 하여 합쳐진 비트열이 된다.
;;   디코딩하기
;;     인코딩된 데이터(비트열)의 비트마다 허프만 나무에서 방향을 정하여 쫓아가고, 나뭇잎이 나오면 해당 문자가 된다.

;; 직접 짜 본것.
(define (decode-my bits tree)
  (define (iter data search-tree result)
    (if (null? data)
        result
        (let ((w-branch (if (= (car data) 0)
                            (left-branch search-tree)
                            (right-branch search-tree))))
          (if (leaf? w-branch)
              (iter (cdr data) tree (append result (list (symbol-leaf w-branch))))
              (iter (cdr data) w-branch result)))))
  (iter bits tree '()))

;; 책에 있는것.
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- CHOOSE-BRANCH" bit))))
  (decode-1 bits tree))

;; decode에 비교하여 decode-my는
;;   - iterative process 이지만 result가 append로 합쳐진다.
;;   - choose-branch 프로시저가 따로 없어 명확하지 않고, 잘못된 비트값이 들어온 경우 처리가 없다.
;; 결론: decode가 더 좋아 보임.

;; sample
(define leaf-A (make-leaf 'A 9))
(define leaf-B (make-leaf 'B 3))
(define leaf-C (make-leaf 'C 1))
(define leaf-D (make-leaf 'D 1))
(define leaf-E (make-leaf 'E 1))
(define leaf-F (make-leaf 'F 1))
(define leaf-G (make-leaf 'G 1))
(define leaf-H (make-leaf 'H 1))
(define sample-huff-tree (make-code-tree leaf-A
                                         (make-code-tree (make-code-tree leaf-B
                                                                         (make-code-tree leaf-C leaf-D))
                                                         (make-code-tree (make-code-tree leaf-E leaf-F)
                                                                         (make-code-tree leaf-G leaf-H)))))
(define origin-data
  '(B A C A D A E A F A B B A A A G A H))
(define encoded-data
  '(1 0 0 0 1 0 1 0 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 0 1 0 0 1 0 0 0 0 0 1 1 1 0 0 1 1 1 1))
(decode encoded-data sample-huff-tree)
(decode-my encoded-data sample-huff-tree)

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(display "make-leaf-set")
(newline)
(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
(display "======")
(newline)

;; ex 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(display "decode")
(newline)
;; (decode sample-message sample-tree) ;; => '(A D A B B C A)
(display "======")
(newline)

;; ex 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;;encode-symbol 프로시저 짜기.
;; - 한 글자를 인코딩하여 비트 리스트를 내놓는 프로시저
;; - tree에 없는 글자가 들어오면 오류 보여주기
(define (encode-symbol sym tree)
  (if (or (null? tree) (leaf? tree))
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (cond ((in? (symbols left) sym)
               (cons 0 (encode-symbol sym left)))
              ((in? (symbols right) sym)
               (cons 1 (encode-symbol sym right)))
              (else (error "NOT FOUND" sym))))))

(define (in? list item)
  (cond ((null? list) #f)
        ((eq? (car list) item) #t)
        (else (in? (cdr list) item))))
;; (encode-symbol 'D sample-tree) ;; => '(1 1 0)

;; ex 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; 가벼운거 2개 골라서 묶기, leaf-set은 차례 매긴 집합이다.
;; 차례 매겨진 상태이므로 제일 앞에거 2개만 가져와서 tree를 만들고 이걸 다시 adjoin-set 한다.
(define (successive-merge leaf-set)
  (define (iter leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (let ((s1 (car leaf-set))
              (r1 (cdr leaf-set)))
          (let ((s2 (car r1))
                (r2 (cdr r1)))
            (iter (adjoin-set (make-code-tree s1 s2) r2))))))
  (iter leaf-set))

;; 입력
;; '((A 4) (B 2) (C 1) (D 1)))
;; 결과
;; (make-code-tree (make-leaf 'A 4)
;;                 (make-code-tree
;;                    (make-leaf 'B 2)
;;                    (make-code-tree (make-leaf 'D 1)
;;                                    (make-leaf 'C 1)))))
(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))

;; ex 2.70
(define rock-huff-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(define rock-message (append '(GET A JOB)
                             '(SHA NA NA NA NA NA NA NA NA)
                             '(GET A JOB)
                             '(SHA NA NA NA NA NA NA NA NA)
                             '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP)
                             '(SHA BOOM)))
(length (encode rock-message rock-huff-tree))
;; 인코딩 필요 bit수 : 84
;; 8개의 낱말이면 하나의 낱말을 표현하는데 3개 bit 필요하므로 위의 message를 encoding 하면 3 * 36 = 108
