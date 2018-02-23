#|
(define (use-solutions eqns unknowns)
  (define (gobble vars values justifications residuals)
    (let ((plunk-premises
           (map (lambda (var)
                  (eq-get var 'plunk-premise))
                vars)))
      (for-each
       (lambda (var value justification plunk-premise)
         (let ((premises
                (lset-difference eq?
                  (apply lset-union eq?
                         (map car justification))
                  plunk-premises))
               (reasons
                (lset-adjoin eq?
                  (apply lset-union eq?
                         (map cadr justification))
                  'solver)))
           (if *debugging-solve*
               (pp `(solved ,var = ,value)))
           (add-content (eq-get var 'plunk-cell)
             (make-tms
              (supported (maybe-symbolic-result value)
			 premises
			 reasons)))))
       vars values justifications plunk-premises)
      (for-each kick-out! plunk-premises)
      (if *debugging-solve*
          (pp `(residuals ,residuals)))
      (set! *posted-equations* residuals)))
  gobble)
|#
