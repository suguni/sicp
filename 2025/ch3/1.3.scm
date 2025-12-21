
;; ex 3.3
(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m p)
    (if (eq? p password)
        (cond ((eq? m 'check) balance)
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (error "Incorrected password")))

  dispatch)

;; ex 3.7
(define (make-joint protected-acc org-pwd alt-pwd)
  (when (protected-acc 'check org-pwd)
    (lambda (m p)
      (if (eq? p alt-pwd)
          (protected-acc m org-pwd)
          (error "Incorrected password")))))

;; (define (make-account-2 balance password)
;;   (define (withdraw amount)
;;     (if (>= balance amount)
;;         (begin (set! balance (- balance amount))
;;                balance)
;;         "Insufficient funds"))
;;   (define (deposit amount)
;;     (set! balance (+ balance amount))
;;     balance)
;;   (define (make-joint alt-pwd)
;;     (lambda (m p)
;;       (check-pwd-dispatch p alt-pwd m)))
;;   (define (dispatch m n)
;;     (cond ((eq? m 'check) balance)
;;           ((eq? m 'withdraw) withdraw)
;;           ((eq? m 'deposit) deposit)
;;           ((eq? m 'make-joint) (make-joint n))
;;           (else (error "Unknown request -- MAKE-ACCOUNT" m))))
;;   (define (check-pwd-dispatch input pwd m)
;;     (if (eq? input pwd)
;;         (dispatch m)
;;         (error "Incorrected password")))
;;   (lambda (m p)
;;     (check-pwd-dispatch p password m)))

;; ex3.8
(define f
  (let ((s 1))
    (lambda (n)
      (if (eq? n 0)
          (set! s 0)
          (set! s (* s n)))
      s)))

(+ (f 0) (f 1)) ;; 0

(+ (f 1) (f 0)) ;; 1

