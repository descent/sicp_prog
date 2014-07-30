; metacircular evaluator
(define input-prompt ";;;M-Eval input:")
(define output-prompt ";;;M-Eval value")

(define apply-in-underlying-scheme apply)
(define (primitive-implementation proc) (car proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))



(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (list-of-values exps env)
  (display "@")
  (display exps)
  (display "@\n")
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (apply procedure arguments)
  (display "ppp---")
  (display procedure)
  (newline)
  (display arguments)
  (display "---ppp---\n")
  ;'ok)
  (apply-primitive-procedure procedure arguments))



(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (lookup-variable-value var env)
  (list +) )

(define (variable? exp) (symbol? exp))

; 4.1.3 Operations on Environment 有提到什麼是環境
(define (eval exp env)
  (display "#")
  (display exp)
  (display "#\n")
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
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
  (display "output: ")
  (display object)
  (newline)
)

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (annouce-output output-prompt)
      (user-print output)))
  'run)
  ;(driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (annouce-output string)
  (newline) (display string) (newline))

(driver-loop)
;(apply-primitive-procedure (list +) (list 1 2))

