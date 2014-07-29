; metacircular evaluator
(define input-prompt ";;;M-Eval input:")
(define output-prompt ";;;M-Eval value")

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (apply procedure arguments)
  (display procedure)
  (newline)
  (display arguments)
  (newline))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; 4.1.3 Operations on Environment 有提到什麼是環境
(define (eval exp env)
  (display "aa\n")
  (display exp)
  (display "\nbb\n")
  (cond ((self-evaluating? exp) exp)
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknows expression type -- EVAL" exp))))


(define (setup-environment)
  (newline)
  (display "i am setup-env")
  (newline)
)

(define the-global-environment (setup-environment))

(define (user-print object)
  (newline)
  (display "i_am_user-print")
  (newline)
)

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (annouce-output output-prompt)
      (user-print output)))
  ('run))
  ;(driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (annouce-output string)
  (newline) (display string) (newline))

(driver-loop)

