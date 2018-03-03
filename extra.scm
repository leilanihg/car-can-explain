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

(let ((unsolved (plunk-variables val)))
           (if (not (null? unsolved))
               ))




(define (use-solutions equations unknowns)
  (define (gobble vars values justifications residuals)
    ;;(set! *posted-equations* residuals)
    (let ((voids '()) (knowns '()) (results '()) (justs '()))
      (for-each
       (lambda (var val just)
         (let ((unsolved (plunk-variables val)))
           (if (not (null? unsolved))
               (for-each                ;unfinished work
                (lambda (eqn)
                  (set! equations (delq eqn equations))
                  (set! *posted-equations*
                        (cons eqn *posted-equations*)))
                (filter
                 (lambda (eqn)
                   (not (null?
                         (lset-intersection eq?
                           unsolved
                           (equation-variables eqn)))))
                 residuals))
               (begin                   ;solved vars
                 (set! voids
                       (cons (eq-get var 'plunk-premise)
                             voids))
                 (set! knowns (cons var knowns))
                 (set! results (cons val results))
                 (set! justs (cons just justs))))))
       vars values justifications)
      (for-each
       (lambda (var val just)
         (let ((premises
                (lset-difference eq?
                  (apply lset-union eq? (map car just))
                  voids))
               (reasons
                (lset-adjoin eq?
                  (apply lset-union eq? (map cadr just))
                  'solver)))
           (if *debugging-solve* (pp `(solved ,var = ,val)))
           (add-content (eq-get var 'plunk-cell)
             (make-tms
              (supported (maybe-symbolic-result val)
                         premises
                         reasons)))))
       knowns results justs)
      (for-each kick-out! voids)
      (if *debugging-solve*
          (pp `(new-posts ,*posted-equations*)))))
  gobble)

(define (use-solutions equations unknowns)
  (define (gobble vars values justifications residuals)
    ;;(set! *posted-equations* residuals)
    (let ((voids '()) (knowns '()) (results '()) (justs '()))
      (for-each
       (lambda (var val just)
         (let ((unsolved (plunk-variables val)))
           (if (not (null? unsolved))
               (for-each                ;unfinished work
                (lambda (eqn)
                  (set! equations (delq eqn equations))
                  (set! *posted-equations*
                        (cons eqn *posted-equations*)))
                (filter
                 (lambda (eqn)
                   (not (null?
                         (lset-intersection eq?
			    unsolved (equation-variables eqn)))))
                 residuals))))
	 (set! voids
	       (cons (eq-get var 'plunk-premise)
		     voids))
	 (set! knowns (cons var knowns))
	 (set! results (cons val results))
	 (set! justs (cons just justs)))
       vars values justifications)
      (for-each
       (lambda (var val just)
         (let ((premises
                (lset-difference eq?
                  (apply lset-union eq? (map car just))
                  voids))
               (reasons
                (lset-adjoin eq?
                  (apply lset-union eq? (map cadr just))
                  'solver)))
           (if *debugging-solve* (pp `(solved ,var = ,val)))
           (add-content (eq-get var 'plunk-cell)
             (make-tms
              (supported (maybe-symbolic-result val)
                         premises
                         reasons)))))
       knowns results justs)
      (for-each kick-out! voids)
      (if *debugging-solve*
          (pp `(new-posts ,*posted-equations*)))))
  gobble)


#|
;;; Exact solution of 1 quasi-linear equation:

(define (solve-one-var-algebraic eqn var succeed fail)
  (isolatable? var eqn succeed fail))

;;; Numerical methods are often better, because floating
;;; point kills symbolic manipulation (polynomial gcd).

(define (solve-one-var-bisection eqn var succeed fail)
  (let ((f (lambda->numerical-procedure
            `(lambda (,var) ,(equation-expression eqn)))))
    (find-a-root f
                 (- root-search-bounds)
                 root-search-bounds
                 root-search-interval
                 bisection-search-tolerance
                 succeed
                 fail)))

(define root-search-bounds 100.0)
(define root-search-interval 0.1)
(define root-search-tolerance 1e-15)

;;; Set this to desired method
(define solve-one-var solve-one-var-algebraic)
;;; (define solve-one-var solve-one-var-bisection)
;;; In any case, gobble up a solution using this method.

(define (use-solution-1 argument var lhs rhs)
  (let ((plunk-premise (eq-get var 'plunk-premise))
        (cell (eq-get var 'plunk-cell)))
    (let ((premises
           (delq plunk-premise
             (apply lset-union eq? (map car argument))))
          (reasons
           (lset-adjoin eq?
             (apply lset-union eq? (map cadr argument))
             'solver)))
      (define (gobble value)
        (let ((val (g:simplify value)))
          `(,var = ,val)
          (kick-out! plunk-premise)
          (add-content cell
                       (supported (maybe-symbolic-result val)
				  premises reasons))
          (maybe-symbolic-result
           (choose-simpler-expression
            (g:simplify (substitute val var lhs))
            (g:simplify (substitute val var rhs))))))
      gobble)))

;;; Numerical solutions of many equations all depend 
;;; on all of the equations, but symbolic solutions 
;;; may be more discriminating as to dependencies.

(define (general-solve-numerical eqns unknowns
                                 succeed
                                 contradiction-failure
                                 inadequate-solver-failure)
             #!optional initial-point initial-step min-step
             tolerance)
  (let ((n (length unknowns))
        (cfail (lambda (dismiss) (contradiction-failure)))
        (ifail (lambda (dismiss) (inadequate-solver-failure))))
    (assert (>= (length eqns) n) "not enuf equations")
    (if (default-object? initial-point)
        (set! initial-point (make-vector n 1.5)))
    (if (default-object? initial-step)
        (set! initial-step (make-vector n 0.1)))
    (if (default-object? min-step)
        (set! min-step (* 10 n *machine-epsilon*)))
    (if (default-object? tolerance)
        (set! tolerance (* 100 n *machine-epsilon*)))
    (let* ((v (generate-uninterned-symbol 'v))
           (f (lambda->numerical-procedure
               `(lambda (,v)
                  (let ,(map (lambda (unknown i)
                               `(,unknown (vector-ref ,v ,i)))
                             unknowns (iota (length unknowns)))
                    (vector ,@(map equation-expression eqns)))))))
      (multidimensional-root-internal f initial-point
                                      initial-step min-step
        (lambda (proposed-root)
          (let* ((justification
                  (map car (map equation-justifications eqns)))
                 (justifications
                  (map (lambda (unk) justification) unknowns)))
            (let ((value (f proposed-root)))
              (if (< (maxnorm value) tolerance)
                  (succeed unknowns
                           (vector->list proposed-root)
                           justifications
                           '())
                  (cfail)))))            ;contradiction failure?
        ifail))))                        ;underdetermined failure?
                                         ; Cannot distinguish!

;;; (define general-solve general-solve-numerical)
|#


          (remove-duplicates
           (append-map
            (lambda (var val)
              (if (null? (plunk-variables val))
                  (list (eq-get var 'plunk-premise))
                  '()))
            now-known-vars their-values))

           #|
           (if (implies-values? v&s2-value merge-value)
               ;; Confirmation of existing information
               (if (more-informative-support? v&s2 v&s1)
                   v&s2
                   v&s1)
               ;; New information is not interesting
               v&s1)
           |#

(lambda ()
  (let ((new-val
         (supported (maybe-symbolic-result val)
                    premises reasons)))
    (pp `(old-val ,(content (eq-get var 'plunk-cell))))
    (pp `(new-val ,new-val))
    (pp `(before
          ,(eq-get var 'plunk-premise)
          = ,(premise-in? (eq-get var 'plunk-premise))))
    (add-content (eq-get var 'plunk-cell) new-val (list 'solver))
    (pp `(after
          ,(eq-get var 'plunk-premise)
          = ,(premise-in? (eq-get var 'plunk-premise))))
    (pp `(after-val ,(content (eq-get var 'plunk-cell))))))

(define (equation-difficulty equation)
  (apply + (map (max-exponent (equation-expression equation))
		(equation-variables equation))))

