(define (error reason . args)
      (display "Error: ")
      (display reason)
      (for-each (lambda (arg) 
                  (display " ")
          (write arg))
        args)
      (newline)
      (scheme-report-environment -1))  ;; we hope that this will signal an error
