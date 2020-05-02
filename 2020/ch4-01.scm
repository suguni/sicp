(define (s/eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-action exp) env))
        ((cond? exp) (s/eval (cond->if exp) env))
        ((application? exp) (s/apply (s/eval (operator exp) env)
                                     (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- S/EVAL" exp))))

(define (s/apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameter procedure)
                                            arguments
                                            (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- S/APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (s/eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; ex 4.1
(define (list-of-values-left exps env)
  (if (no-operands? exps)
      '()
      (let ((operand (s/eval (first-operand exps) env)))
        (cons operand
              (list-of-values-left (rest-operands exps) env)))))

(define (list-of-values-right exps env)
  (if (no-operands? exps)
      '()
      (let ((operands (list-of-values-right (rest-operands exps) env)))
        (cons (s/eval (first-operand exps) env) operands))))

(define (list-of-values-right exps)
  (if (null? exps)
      '()
      (let ((rest (list-of-values-right (cdr exps))))
        (cons (begin (display (car exps)) (car exps)) rest))))

(define (eval-if exp env)
  (if (true? (s/eval (if-predicate exp) env))
      (s/eval (if-consequent exp) env)
      (s/eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (s/eval (first-exp exps) env))
        (else (s/eval (first-exp exps) env)
              (eval-sequences (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (s/eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (s/eval (definition-value exp) env)
                    env)
  'ok)