;; 3.3.4 A simulator for digital circuits

;(define a (make-wire))
;(define b (make-wire))
;(define c (make-wire))
;
;(define d (make-wire))
;(define e (make-wire))
;(define s (make-wire))
;
;(or-gate a b d)
;(and-gate a b c)
;(inverter c e)
;(and-gate d e s)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;;; full-adder 사용은? 아래와 같이 하면 될까?
;(define in1 (make-wire))
;(set-signal! in1 1)
;(define in2 (make-wire))
;(set-signal! in2 1)
;(define sum (make-wire))
;(define c (make-wire))
;(full-adder in1 in2 sum c)
;;; 이후 sum, c 확인
;
;;; 아니면..
;(define in1 (make-wire))
;(define in2 (make-wire))
;(define sum (make-wire))
;(define c (make-wire))
;(full-adder in1 in2 sum c)
;(set-signal! in1 1)
;;; sum, c 확인
;(set-signal! in2 1)
;;; sum, c 확인
;(set-signal! in2 0)
;;; sum, c 확인
;;; 이후 sum, c 확인
;;; 이렇게 하면 이후 in1, in2 를 변경할때마다 full-adder 를 실행하지 않고도 sum, c 를 확인할 수 있다.
;;; 회로(full-adder)는 고정된 것이므로, 즉 full-adder 프로시저가 물체이므로 이런 논리가 더 적당하다.


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a1 a2)
  (cond ((not (or (valid-signal? a1) (valid-signal? a2))) (error "Invalid signal" a1 a2))
        ((and (= a1 1) (= a2 1)) 1)
        (else 0)))

(define (valid-signal? s)
  (or (= s 1) (= s 0)))

;; ex 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a1 a2)
  (cond ((not (or (valid-signal? a1) (valid-signal? a2))) (error "Invalid signal" a1 a2))
        ((or (= a1 1) (= a2 1)) 1)
        (else 0)))

;; ex 3.29
(define (or-gate-2 a1 a2 output)
  (let ((i1 (make-wire))
        (i2 (make-wire))
        (e1 (make-wire)))
    (inverter a1 i1)
    (inverter a2 i2)
    (and-gate i1 i2 e1)
    (inverter e1 output)))
;; delay = (+ (* 3 inverter-delay) and-gate-delay)
;; 아니다.
;;
;; a1 또는 a2가 변경될때 하나의 인버터로부터 output 까지의 signal만 변경되므로
;; delay = (+ (* 2 inverter-delay) and-gate-delay)
;; 가 된다.

;; ex 3.30
;(define (ripple-carry-adder a-wires b-wires s-wires c)
;  
;  (define (make-wires count)
;    (define (iter wires n)
;      (if (= 0 n)
;          wires
;          (iter (cons (make-wire) wires) (- n 1))))
;    (iter '() count))
;
;  (define (iter a b c-in s c-out)
;    (if (null? a)
;        'ok
;        (begin
;          (full-adder (car a) (car b) c-in (car s) (car c-out))
;          (iter (cdr a) (cdr b) (car c-out) (cdr s) (cdr c-out)))))
;  
;  (let ((c-out-wires (make-wires (length a-wires))))
;    (iter a-wires b-wires c s-wires c-out-wires)))

;; half-adder-delay <= (+ or-delay inverter-delay (* 2 and-delay))
;; full-adder-delay <= (+ (* 2 half-adder-delay) or-delay)
;; ripple-carry-adder-delay <= (* n full-adder-delay)
;;                           = (* n (+ (* 2 (+ or-delay inverter-delay (* 2 and-delay))) or-delay))

(define (ripple-carry-adder a-wires b-wires s-wires c)  
  (define (iter a b s c-out)
    (let ((c-in (make-wire)))
      (if (null? a)
          'ok
          (begin
            (full-adder (car a) (car b) c-out (car s) c-in)
            (iter (cdr a) (cdr b) c-in (cdr s))))))
  (iter a-wires b-wires s-wires c))

;; 위 방법은 imperative 한 코드이다. recursive 하게 만들 필요 있다.

;; half-adder-delay <= (+ or-delay inverter-delay (* 2 and-delay))
;; full-adder-delay <= (+ (* 2 half-adder-delay) or-delay) - b가 변경되었을때
;;                     (+ half-adder-delay or-delay)       - a가 변경되었을때
;; ripple-carry-adder-delay <= 최대 : (* n full-adder-delay) = (* n (+ (* 2 half-adder-delay) or-delay))
;;                             최소 : (* 1 full-adder-delay) = (+ half-adder-delay or-delay)


;; p360 줄만들기
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))

    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc)) ;; 이거 왜 할까? - 최초 호출 시 실행될 필요 있음. - ex 3.31
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

;; p363 시간표
;; 이상한데... the-agenda 뭥미..

(define (after-delay delay proc)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  proc
                  the-agenda))

;; 어디 쓰지?
(define (propagate)
  (let ((cnt 0))
    (define (iter)
      (if (empty-agenda? the-agenda)
          'done
          (begin
            (set! cnt (+ cnt 1))
            ((first-agenda-item the-agenda))      ;; 첫번째 item 실행
            (remove-first-agenda-item! the-agenda) ;; 첫번째 item 삭제
            (display the-agenda)
            (newline)
            (iter))))
    (display the-agenda)
    (newline)
    (iter)
    (display "count : ")
    (display cnt )))

;; p364 시뮬레이션 해 보기.
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire))
                 (newline))))

;; ex 3.31
;; make-wire 안쪽의 accept-action-procedure!는 입력된 프로시저를 실행하고 있는데 왜?
;;
;;   inverter 프로시저가 실행되면 실행될때의 input에 따라 output이 변경되어야 함
;;   즉, (inverter wire1 wire2) 로 실행하면, wire1과 wire2로 연결된 inverter 물체가 생기면서
;;       wire1 상태에 따라 inverter가 동작하여 wire2의 값이 변해야 함.
;;       위 프로시저를 실행하는 코드의 구현은 wire1이 변경될 때 실행될 프로시저를 add-action!으로 추가하고 있는데
;;       이 프로시저가 최초 실행시에도 실행되어야 하는 것임. 
;;
;; accept-action-procedure! 를 아래처럼 변경하면 수정해야 할 코드
;;  (define (accept-action-procedure! proc)
;;    (set! action-procedures (cons proc action-procedures)))
;; 
;;   기본 소자(inverter, and-gate, or-gate)를 정의하는 프로시저에서,
;;   add-action 호출 이후 실제 output을 변경하는 프로시저를 한번 호출해야 함
;;   아래와 같음.
;;   (define (inverter input output)
;;     (define (invert-input)
;;       (let ((new-value (logical-not (get-signal input))))
;;         (after-delay inverter-delay
;;                      (lambda () (set-signal! output new-value)))))
;;     (add-action! input invert-input)
;;     (invert-input) ;; 여기!!!!!!
;;     'ok)

;; p366 시간표 만들기

;; 3.3.2 의 queue 사용
(load "ch3.3.2-suguni.rkt")

(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  
  (define (belongs-before? segments time)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments time action)
    (let ((segment (car segments)))
      (if (= time (segment-time segment))
          (insert-queue! (segment-queue segment) action)
          (let ((rest (cdr segments)))
            (if (belongs-before? rest time)
                (set-cdr! segments
                          (cons (make-new-time-segment time action) rest))
                (add-to-segments! rest time action))))))
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segments time) ;; 현재 segments 보다 앞선 time 인지?
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments time action))))

(define (remove-first-agenda-item! agenda)
  (if (not (empty-agenda? agenda))
      (let ((q (segment-queue (first-segment agenda))))
        (delete-queue! q)
        (if (empty-queue? q)
            (set-segments! agenda (rest-segments agenda))))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST AGENDA-ITEM")
      (let ((segment (first-segment agenda)))
        (set-current-time! agenda (segment-time segment))
        (front-queue (segment-queue segment)))))

;; p364 시뮬레이션 해 보기
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)
(probe 'input-1 input-1)
(probe 'input-2 input-2)

;; (and-gate input-1 input-2 sum)
;; (inverter sum carry)
(half-adder input-1 input-2 sum carry)

;; (set-signal! input-1 1)
;; (propagate)

;; (newline)
;; (get-signal sum)
