; metacircular evaluator
(define input-prompt ";;;M-Eval input:")
(define output-prompt ";;;M-Eval value")

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
  (newline) (display string) (newline)


