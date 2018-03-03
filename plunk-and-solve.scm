;;; ------------------------------------------------------------------
;;; Copyright 2016 Alexey Radul and Gerald Jay Sussman
;;; ------------------------------------------------------------------
;;; This file is part of New Propagator Prototype.  It is derived from
;;; the Artistic Propagator Prototype previously developed by Alexey
;;; Radul and Gerald Jay Sussman.
;;; 
;;; New Propagator Prototype is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;; 
;;; New Propagator Prototype is distributed in the hope that it will
;;; be useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with New Artistic Propagator Prototype.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;; ------------------------------------------------------------------

;;; Wallpaper

(define *debugging-equate* #f)

(define *debugging-general-solve* #f)


;;; The substitutions accumulated by solving equations are collected
;;; here.  This is initialized by the scheduler.

(define *substitutions* '())

;;; Every access of a symbolic value must go through this:

(define (current-best-value symb-v&s)
  (let* ((symb-v&s (->v&s symb-v&s))  
         (expression (v&s-value symb-v&s))
         (premises (v&s-support symb-v&s))
         (reasons (v&s-reasons symb-v&s)))
    (let ((expr 
           (fold-left (lambda (expr sub)
                         (s:simplify
                          (substitute (substitution-expression sub)
                                      (substitution-variable sub)
                                      expr)))
                       expression
                       *substitutions*))
          (s&rs
           (map (lambda (sub)
                  (solver-justifications->s&r
                   (substitution-justifications sub)))
                *substitutions*)))
      (let ((all-premises
             (fold-left (lambda (supp sub-s&r)
                           (lset-union equal? 
                                       (argument-supports sub-s&r)
                                       supp))
                         premises
                         s&rs))
            (all-reasons
             (fold-left (lambda (reas sub-s&r)
                           (lset-union equal? 
                                       (argument-reasons sub-s&r)
                                       reas))
                         reasons
                         s&rs)))
        (supported (maybe-symbolic-result expr)
                   all-premises
                   all-reasons)))))

;;; A plunk will cause propagation of symbolic expressions.
;;; Eventually these will collide in MERGE, causing EQUATE!  to be
;;; invoked.

(define (plunk! cell)
  (assert (cell? cell) "Can only plunk a cell.")
  (let* ((var (make-plunk-variable (name cell)))
	 (premise (symbol "premise-" var)))
    (eq-put! premise 'premise #t)
    (eq-put! var 'plunk-premise premise)
    (eq-put! premise 'plunk-var var)
    (eq-put! var 'plunk-cell cell)
    (add-content cell
      (make-tms
       (supported (literal-number var)
		  (list premise)
		  (list '(plunker)))))
    (bring-in! premise)
    (run)))

(define (make-plunk-variable cell-name)
  (set! *plunk-counter* (+ *plunk-counter* 1))
  (string->symbol 
   (string-append
    (fold-left string-append
               ""
               (map symbol->string cell-name))
    "_"
    (number->string *plunk-counter*))))

(define *plunk-counter* 0)

(define (plunk-variable? sym)
  (eq-get sym 'plunk-premise))

(define *numeric-merge-tolerance* 1e-10)
(define *symbolic-merge-tolerance* 1e-5)

(define (equate-v&s! v&s1 v&s2)
  (cond ((not (all-premises-in? (v&s-support v&s2)))
         v&s1)
        ((not (all-premises-in? (v&s-support v&s1)))
         v&s2)
        (else
         (if *debugging-equate* (pp `(equate! ,v&s1 ,v&s2)))
         (let* ((v&s1 (current-best-value v&s1))
                (v&s2 (current-best-value v&s2))
                (supports (merge-supports v&s1 v&s2))
                (reasons (merge-reasons v&s1 v&s2))
                (lhs (v&s-value v&s1))
                (rhs (v&s-value v&s2)))
           (define (make-result expr)
             (supported expr supports reasons))
           (cond ((equal? lhs rhs) v&s1)
                 ((and (number? lhs) (number? rhs))
                  (if (default-equal? lhs rhs *numeric-merge-tolerance*)
                      v&s1
                      (make-result the-contradiction)))
                 (else
                  (let ((residual (g:simplify (symb:- lhs rhs)))
                        (default-result
                          (maybe-symbolic-result
                           (choose-simpler-expression lhs rhs))))
                    (if (and (number? residual)
                             (default-equal? residual 0
                               *symbolic-merge-tolerance*))
                        (make-result default-result)
                        (let ((vars (plunk-variables residual)))
                          (if (null? vars)
                              (make-result the-contradiction)
                              (general-solve
                               (list ; Note: this is one equation only!
                                ;; Probably should add this new equation
                                ;; to previously unsolved stuff, if any.
                                (make-equation residual 
                                 (list  ;Solver equation has list of justs.
                                  (s&r->solver-justification supports 
                                                             reasons))))
                               vars          ;unknowns
                               (use-solutions supports
                                              reasons
                                              default-result)
                               (lambda ()
                                 (make-result the-contradiction))
                               (lambda ()
                                 (make-result default-result)))))))))))))

(define (abstract-v&s? x)
  (and (v&s? x)
       (abstract-number? (v&s-value x))))

(assign-operation 'merge equate-v&s! abstract-v&s? abstract-v&s?)

(assign-operation 'merge equate-v&s! abstract-v&s? flat-v&s?)
(assign-operation 'merge equate-v&s! flat-v&s? abstract-v&s?)


(define (equate-number-v&s x v&s)
  (equate-v&s! (->v&s x) v&s))

(assign-operation 'merge equate-number-v&s number? abstract-v&s?)


(define (equate-v&s-number v&s x)
  (equate-v&s! v&s (->v&s x)))

(assign-operation 'merge equate-v&s-number abstract-v&s? number?)


(define (abstract-tms? x)
  (and (tms? x)
       (abstract-v&s? (strongest-consequence x))))

(define (equate-tms-tms tms1 tms2) 
  (merge (tms-query tms1) (tms-query tms2)))

(define (equate-tms-v&s tms v&s)
  (merge (tms-query tms) v&s))

(define (equate-v&s-tms v&s tms)
  (merge v&s (tms-query tms)))

(assign-operation 'merge equate-tms-tms abstract-tms? abstract-tms?)
(assign-operation 'merge equate-tms-v&s abstract-tms? flat-v&s?)
(assign-operation 'merge equate-v&s-tms flat-v&s? abstract-tms?)

;;; Equations to be solved by the Scmutils solver must have an
;;; expression, a list of justifications (symbols used for tracking
;;; which equations contributed to each substitution), and variables.

;;; Propagator v&s values have two components to a justification: the
;;; set of premises that support the expression and a reason, which
;;; gives the immediate propagator that produced the expression.

;;; So to make this work we must prepare equations for the solver by
;;; making up solver justifications that pack up propagator
;;; justifications.

(define (s&r->solver-justification supports reasons)
  (let ((supports (sort supports expr:<)) ;canonicalize order
        (reasons (sort reasons expr:<)))
    (let ((key (list supports reasons)))
      (let ((seen (assoc key *justification-memory*)))
        (if seen
            (cdr seen) 
            (let ((j (generate-uninterned-symbol "J")))
              (set! *justification-memory*
                    (cons (cons key j) *justification-memory*))
               j))))))
                        
;;; We also need to be able to unpack solver justifications into
;;; propagator justifications.
;;; (define rassoc (association-procedure equal? cdr))

;;; Given a list of solver justifications (and other premises) we need
;;; to produce the supports and reasons needed by the propagator
;;; system.

(define (solver-justifications->s&r js)
  (let lp ((js js) (supports '()) (reasons '()))
    (if (null? js)
        (list supports reasons)
        (let ((seen (rassoc (car js) *justification-memory*)))
          (if seen
              (lp (cdr js) 
                  (lset-union equal? (caar seen) supports)
                  (lset-union equal? (cadar seen) reasons))
              (lp (cdr js)
                  (lset-adjoin equal? supports (car js))
                  reasons))))))

(define (argument-supports argument) (car argument))

(define (argument-reasons argument) (cadr argument))

(define (general-solve eqns unknowns
                       succeed
                       contradiction-failure
                       inadequate-solver-failure)
  (if *debugging-general-solve* (pp `(solving ,eqns ,unknowns)))
  (let ((solve-result (solve-equations eqns unknowns)))
    (case (car solve-result)
      ((full-solutions underdetermined) 
       (if *debugging-general-solve* (pp `(solved ,solve-result)))
       ;; ASSUMPTION: there is a value to be returned that is a
       ;; value of the cell whose merge discovered the equation.
       ;; All of these should be returned to the cell and merged.
       (if (null? (cdr solve-result)) (error "Huh?"))
       (let lp ((more (cddr solve-result)) 
                (value (succeed (substitutions (cadr solve-result)))))
         (if (null? more)
             (begin
               (if *debugging-general-solve*
                   (pp `(value-returned ,value)))
               value)
             (lp (cdr more)
                 (merge (succeed (substitutions (car more)))
                        value)))))
      ((contradictions)
       (if *debugging-general-solve* (pp 'contradiction-1))
       (contradiction-failure))
      ((parameters-constrained tough-equations extra-equations)
       ;; Some equations should be added to "unsolved" for later,
       ;; see equate-v&s! above.
       (if *debugging-general-solve* (pp solve-result))
       (inadequate-solver-failure))
      (else (error "Unknown result" solve-result)))))


(define ((use-solutions premises reasons default-result) substitutions)
  (if *debugging-general-solve* (pp `(using ,substitutions)))
  (let* ((now-known-vars
          (map substitution-variable substitutions))
         (their-values
          (map substitution-expression substitutions))
         (their-justifications
          (map (compose solver-justifications->s&r
                        substitution-justifications)
               substitutions))
         (premises-to-be-retracted
          (append-map
           (lambda (var val) 
             (if (null? (plunk-variables val))
                 (list (eq-get var 'plunk-premise))
                 '()))
           now-known-vars their-values))
         (new-substitutions 
          (map
           (lambda (var val justs)
             (let* ((premises
                     (lset-difference equal?
                                      (argument-supports justs)
                                      premises-to-be-retracted))
                    (reasons
                     (lset-adjoin equal?
                                  (argument-reasons justs)
                                  '(solver))))
               (if *debugging-general-solve*
                   (pp `(plunk-resolved ,var = ,val)))
               (alert-propagators       ;delayed add-content.
                (lambda ()
                  (let ((new-val
                         (supported (maybe-symbolic-result val)
                                    premises reasons)))
                    (add-content (eq-get var 'plunk-cell)
                                 new-val
                                 (list 'solver)))))
               (make-substitution var val 
                 (list (s&r->solver-justification premises reasons)))))
           now-known-vars their-values their-justifications))
         ;; let* continued on next page.

         ;; let* continued from previous page.
         (updated-old-substitutions
          (map
           (lambda (old-substitution)
             (fold-left
              (lambda (old new)
                (if (occurs? (substitution-variable new)
                             (substitution-expression old))
                    (make-substitution
                     (substitution-variable old)
                     (substitute (substitution-expression new)
                                 (substitution-variable new)
                                 (substitution-expression old))
                     (just-union 
                      (substitution-justifications new)
                      (update-justifications premises-to-be-retracted
                                             old)))
                    (make-substitution
                     (substitution-variable old)
                     (substitution-expression old)
                     (update-justifications premises-to-be-retracted
                                            old))))
              old-substitution
              new-substitutions))
           *substitutions*)))
    (if *debugging-general-solve*
        (pp `(entering-substitutions ,*substitutions*)))
    (set! *substitutions* 
          (lset-union equal?
                      new-substitutions updated-old-substitutions))
    (if *debugging-general-solve*
        (pp `(exiting-substitutions ,*substitutions*)))
    (if *debugging-general-solve*
        (pp `(to-be-retracted ,premises-to-be-retracted)))
    (for-each kick-out! premises-to-be-retracted)    
    (current-best-value
     (supported (maybe-symbolic-result default-result)
                (lset-difference equal?
                                 premises
                                 premises-to-be-retracted)
                reasons))))

(define (update-justifications premises-to-be-retracted old)
  (let ((justs (solver-justifications->s&r
                (substitution-justifications old))))
    (list (s&r->solver-justification
           (lset-difference equal?
                            (argument-supports justs)
                            premises-to-be-retracted)
           (argument-reasons justs)))))

(define (choose-simpler-expression lhs rhs)
  (cond ((number? lhs) lhs)
        ((number? rhs) rhs)
        (else
         (let ((vlhs (plunk-variables lhs))
               (vrhs (plunk-variables rhs)))
           (let ((elhs (map (max-exponent lhs) vlhs))
                 (erhs (map (max-exponent rhs) vrhs)))
             (let ((wlhs (apply + elhs))
                   (wrhs (apply + erhs)))
               (cond ((< wlhs wrhs) lhs)
                     ((< wrhs wlhs) rhs)
                     (else lhs))))))))

(define ((max-exponent expression) var)
  (let lp ((expr expression))
    (cond ((null? expr) 0)
	  ((equal? expr var) 1)
	  ((expt? expr)
	   (if (equal? (car (operands expr)) var)
	       (cadr (operands expr))
	       0))
	  ((list? expr)
	   (apply max (map lp expr)))
	  (else 0))))

(define (plunk-variables expr)
  (cond ((pair? expr)
         (lset-union equal?
                     (plunk-variables (car expr))
                     (plunk-variables (cdr expr))))
        ((plunk-variable? expr)
         (list expr))
        (else '())))

(define (maybe-symbolic-result expr)
  (if (or (numeric? expr) (literal-number? expr))
      expr
      (literal-number expr)))

(define (sign-of-abstract-number x)
  (let ((n (g:simplify x)))
    (if (number? n)
        (sign-of-number n)
        nothing)))

(assign-operation 'generic-sign
                  sign-of-abstract-number
                  abstract-number?)

(define (abs-of-abstract-number x)
  (let ((n (g:simplify x)))
    (if (number? n)
        (n:abs n)
        nothing)))

(assign-operation 'abs
                  abs-of-abstract-number
                  abstract-number?)

(define *symbolic-equality-acceptance-tolerance* 1e-4)

(define (symbolic-equal? x y)
  (let ((nx (g:simplify x)) (ny (g:simplify y)))
    (let ((diff (g:simplify (symb:- nx ny))))
      (and (number? diff)
	   (default-equal? diff 0
	     *symbolic-equality-acceptance-tolerance*)))))

(assign-operation 'generic-=
                  symbolic-equal?
                  abstract-number? abstract-number?)

(assign-operation 'generic-=
                  symbolic-equal?
                  number? abstract-number?)
(assign-operation 'generic-=
                  symbolic-equal?
                  abstract-number? number?)

(define (trivially-equivalent? r1 r2)
  (let ((nr1 (g:simplify r1)) (nr2 (g:simplify r2)))
    (or (equal? r1 r2)
	(if (and (number? nr1) (number? nr2))
	    (default-equal? nr1 nr2
			    *equation-residual-tolerance*)
	    (let ((quo (g:simplify (symb:/ nr1 nr2))))
	      (number? quo))))))
