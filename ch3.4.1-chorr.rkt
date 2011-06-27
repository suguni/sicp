;; 언어 R5RS 설정필요
;; ch 3.4.1 병행 시스템에서 시간의 성질(본질)

;; 연습문제 3.38

;; a) 
(define balance 100)
(set! balance (+ balance 10))
(set! balance (- balance 20))
(set! balance (- balance (/ balance 2)))
(display balance)(newline)
;((100 + 10) - 20) / 2 = 45
(define balance 100)
(set! balance (- balance 20))
(set! balance (+ balance 10))
(set! balance (- balance (/ balance 2)))
(display balance)(newline)
;((100 - 20) + 10) / 2 = 45
(define balance 100)
(set! balance (+ balance 10))
(set! balance (- balance (/ balance 2)))
(set! balance (- balance 20))
(display balance)(newline)
;((100 + 10) / 2) - 20 = 35
(define balance 100)
(set! balance (- balance 20))
(set! balance (- balance (/ balance 2)))
(set! balance (+ balance 10))
(display balance)(newline)
;((100 - 20) / 2) + 10 = 50
(define balance 100)
(set! balance (- balance (/ balance 2)))
(set! balance (+ balance 10))
(set! balance (- balance 20))
(display balance)(newline)
;((100 / 2) + 10) - 20 = 40
(define balance 100)
(set! balance (- balance (/ balance 2)))
(set! balance (- balance 20))
(set! balance (+ balance 10))
(display balance)(newline)
;((100 / 2) - 20) + 10 = 40

;; b)
; [예시] (100+10) 연산되어 set! 되는 순간 (100-20) 연산 결과가 set! 된다 >> balance = 80
; 그 후 Mary의 연산이 시행되면 40 이 나올 수 있다.
