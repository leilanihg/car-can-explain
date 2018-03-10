;; For explaining v&s values

(define generic-pp (make-generic-operator 1 'pp pp))

(define (pp-propagator propagator)
  (display "function ")
  (display (propagator-short-name propagator))
  (display "\n")
  (display "    inputs:\n")
  (for-each (lambda (input)
	      (display "    ")
	      (display-cell-name input)
	      (display "\n"))
	    (propagator-inputs propagator))
  (display "\n    outputs:\n")
  (for-each (lambda (output)
	      (display "    ")
	      (display-cell-name output)
	      (display "\n"))
	    (propagator-outputs propagator))
  (display "\n"))

(assign-operation 'pp pp-propagator propagator?)


(define anonymous-propagator-name (list '*anonymous-propagator*))

(define (propagator-short-name propagator)
  (propagator 'short-name))
