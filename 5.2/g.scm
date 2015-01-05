(define (assemble controller-text)
  (extract-labels controller-text
    (lambda (insts labels)
  (display "\ninsts/labels: +++\n");
  (display insts)
  (display "\n")
  (display labels)
  (display "\n++++++\n");
      ))) 

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (make-instruction text)
  (cons text '()))

(assemble
  '(
     test-b  
     (assign a (reg b))
     (assign b (reg t))
     gcd-done
   )
)

