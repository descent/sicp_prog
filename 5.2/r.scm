;(define gcd-machine 
;  (make-machine 
;    '(a b t)
;    (list (list 'rem remainder) (list '= =))
;    '(test-b  
;  (test (op = ) (reg b ) (const 0))
;  (branch (label gcd-done))
;  (assign t (op rem) (reg a ) (reg b))
;  (assign a (reg b))
;  (assign b (reg t))
;  (goto (label test-b))
; gcd-done)))



(define (extract-labels text)
  (if (null? text)
      (cons '() '())
      (let ((result (extract-labels (cdr text))))
        (let ((insts (car result)) (labels (cdr result)))
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (cons insts
                      (cons (make-label-entry next-inst insts) labels))
                (cons (cons (make-instruction next-inst) insts)
                      labels))))) ))


;(define (extract-labels text)
;        (display "\n\!!\n")
;        (display text)
;        (display "\n\!!!!\n")
;  (if (null? text)
;      (cons '() '())
;      (let ((result (extract-labels (cdr text))))
;        (display "\n\!!\n")
;        (display result)
;        (display "\n\!!!!\n")
;        result
;        )))

(extract-labels '(test-b (test (op = ) (reg b ) (const 0))))

;(define gcd-machine 
;  (make-machine 
;    '(a b t)
;    (list (list 'rem remainder) (list '= =))
;    '(test-b  
;  (test (op = ) (reg b ) (const 0))
; )))

;(set-register-contents! gcd-machine 'a 206)
;(set-register-contents! gcd-machine 'b 40)
;(start gcd-machine)
;(get-register-contents gcd-machine 'a)


