#lang racket

(define (memq item x)
  (cond ((null? x) false)
        ((eq? (car x) item) x)
        (else (memq item (cdr x)))))

;(memq 'apple '(pear banana prune))
;(memq 'apple '(x (apple sauce) y apple pear))

;; ex 2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

;; ex 2.54
(define (equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        (else #f)))

;; ex 2.55
(car ''abracadabra)
;; > (car (quote (quote abracadabra))) > (car (list 'quote 'abracadabra)) > 'quote
