; metacircular evaluator
(define input-prompt ";;;M-Eval input:")
(define output-prompt ";;;M-Eval value")

; 4.1.3 Operations on Environment 有提到什麼是環境
(define (eval exp env)
  (newline)
  (display "i am eval")
  (newline)
)

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
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (annouce-output string)
  (newline) (display string) (newline))

(driver-loop)

