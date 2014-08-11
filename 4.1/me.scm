; metacircular evaluator
(define input-prompt ";;;M-Eval input:")
(define output-prompt ";;;M-Eval value")

(define apply-in-underlying-scheme apply)
(define (primitive-implementation proc) (cadr proc))

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

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (apply procedure arguments)
  (display "\n---proc---\n")
  (display procedure)
  (display "\n---args---\n")
  (display arguments)
  (display "------\n")
  ;'ok)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))



(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;(define (lookup-variable-value var env)
;  (list +) )

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (lookup-variable-value var env)
  (display "\n!!!")
  (display var)
  (newline)
  (display env)
  (display "!!!\n")
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (variable? exp) (symbol? exp))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (make-lambda parameters body)
  (display (cons 'lambda (cons parameters body)))
  (cons 'lambda (cons parameters body)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

; (lambda (x) (+ x 4))
; ((lambda (x) (+ x 4)) 5)
; 4.1.3 Operations on Environment 有提到什麼是環境
(define (eval exp env)
  (display "#")
  (display exp)
  (display "#\n")
  (cond ((self-evaluating? exp) exp)
        ((definition? exp) (eval-definition exp env))
        ((variable? exp) (lookup-variable-value exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) 
                         (lambda-body exp) 
                         env))

        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknows expression type -- EVAL" exp))))

(define (make-frame variables values)
  (cons variables values))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define the-empty-environment '())

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

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
  ;'run)
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (annouce-output string)
  (newline) (display string) (newline))

;(apply-primitive-procedure (list +) (list 1 2))
;(display the-global-environment)

;(display "***") (display (lookup-variable-value '+ the-global-environment)) (display "***\n")

(driver-loop)
