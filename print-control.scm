(define (->significant-figures places number)
  (string->number
   (let-fluids flonum-unparser-cutoff `(relative ,places normal)
               (lambda () (number->string number)))))

(define (sane-pp expression output-port)
  ;; Random parametrization.
  (let-fluids *unparser-list-breadth-limit* 5
              *unparser-list-depth-limit* 3
              *unparser-string-length-limit* 40
              *unparse-primitives-by-name?* #t
              *pp-save-vertical-space?* #t
              *pp-default-as-code?* #t
    (lambda () (pp expression output-port))))
