;;; ----------------------------------------------------------------------
;;; Copyright 2016 Alexey Radul and Gerald Jay Sussman
;;; ----------------------------------------------------------------------
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
;;; ----------------------------------------------------------------------

(declare (usual-integrations make-cell))

(define-structure
 (v&s (named 'supported) (type list)
      (constructor %supported (value support #!optional reasons))
      (print-procedure #f))
 value support (reasons (list *my-parent*)))

(define (supported value support #!optional reasons)
  (if (and (pair? value)
           (not (or (nothing? value)
                    (eq? value the-contradiction)
                    (boolean? value)
                    (interval? value)
                    (abstract-number? value))))
      (bkpt "supported"))
  (if (default-object? reasons)
      (%supported value support)
      (%supported value support reasons)))

;;; Should be generic.
(define (implies-values? v1 v2)
  (cond ((nothing? v1) #f)
        ((nothing? v2) #t)
        ((contradiction? v1) #t)
        ((contradiction? v2) #f)
        ((and (boolean? v1) (boolean? v2))
         (eq? v1 v2))
        (;; v1 implies v2 iff v1=v2.
         (and (number? v1) (number? v2))
         (default-equal? v1 v2))
        (;; v1 implies v2 iff v1=v2.
         (or (abstract-number? v1) (abstract-number? v2))
         (equivalent-values? v1 v2))
        ((and (interval? v1) (interval? v2))
         (subinterval? v1 v2))
        ((and (interval? v1) (number? v2))
         #f)
        ((and (number? v1) (interval? v2))
         (within-interval? v1 v2))
        (else
         (error "Implies? is confused" v1 v2))))

(define equivalent-values? generic-=)

(define (equivalent-v&ss? v&s1 v&s2)
  (or (eq? v&s1 v&s2)
      (and (or (v&s? v&s1) (v&s? v&s2))
           (let ((v&s1 (->v&s v&s1)) (v&s2 (->v&s v&s2)))
             (and (equivalent-values? (v&s-value v&s1)
                                      (v&s-value v&s2))
                  (lset= eq?
                         (v&s-support v&s1)
                         (v&s-support v&s2))
                  (lset= equal?
                         (v&s-reasons v&s1)
                         (v&s-reasons v&s2)))))))

(define (more-informative-support? v&s1 v&s2)
  (and (not (lset= eq? (v&s-support v&s1) (v&s-support v&s2)))
       (lset<= eq? (v&s-support v&s1) (v&s-support v&s2))))

(define (subsumes-v&s? v&s1 v&s2)
  (and (implies-values? (v&s-value v&s1) (v&s-value v&s2))
       (lset<= eq? (v&s-support v&s1) (v&s-support v&s2))))

(define (v&s-merge v&s1 v&s2)
  (let* ((v&s1-value (v&s-value v&s1))
         (v&s2-value (v&s-value v&s2))
         (value-merge
          (merge v&s1-value v&s2-value)))
    (cond ((equivalent-values? value-merge v&s1-value)
           (if (implies-values? v&s2-value value-merge)
               ;; Confirmation of existing information
               (if (more-informative-support? v&s2 v&s1)
                   v&s2
                   v&s1)
               ;; New information is not interesting
               v&s1))
          ((equivalent-values? value-merge v&s2-value)
           ;; New information overrides old information
           v&s2)
          (else
           ;; Interesting merge, need both provenances
           (supported value-merge
                      (merge-supports v&s1 v&s2)
                      (merge-reasons v&s1 v&s2))))))

(define (merge-supports . v&ss)
  (apply lset-union eq? (map v&s-support v&ss)))

(define (merge-reasons . v&ss)
  (apply lset-union equal? (map v&s-reasons v&ss)))

(assign-operation 'merge v&s-merge v&s? v&s?)

(assign-operation
 'true? (lambda (v&s) (generic-true? (v&s-value v&s))) v&s?)

(define (v&s-contradictory? v&s)
  (contradictory? (v&s-value v&s)))

(assign-operation 'contradictory? v&s-contradictory? v&s?)


(define (v&s-nothingness? v&s)
  (nothingness? (v&s-value v&s)))

(assign-operation 'nothingness? v&s-nothingness? v&s?)

#|
(define (flat? thing)
  (or (interval? thing)
      (number? thing)
      (boolean? thing)))
|#

(define (flat? thing)
  (or (interval? thing)
      (numerical-quantity? thing)
      (boolean? thing)))

(define ((v&s-unpacking f) . args)
  (v&s-unpacker f args))

(define ((v&s-unpacking-and-coercing f) . args)
  (v&s-unpacker f (map ->v&s args)))

(define (v&s-unpacker f args)
  (let ((v (apply f (map v&s-value args))))
    (supported v
               (apply merge-supports args)
               (list *my-parent*))))

(define (->v&s thing)
  (if (v&s? thing)
      thing
      (supported thing
		 '()
		 (list *my-parent*))))

;;; Primitive generic operators are known here--ugh!

(for-each
 (lambda (name underlying-operation)
   (assign-operation name
                     (v&s-unpacking underlying-operation)
                     v&s? v&s?)
   (assign-operation name
                     (v&s-unpacking-and-coercing underlying-operation)
                     v&s? flat?)
   (assign-operation name
                     (v&s-unpacking-and-coercing underlying-operation)
                     flat? v&s?))
 '(+ - * /
   generic-= generic-< generic-> generic-<= generic->=
   and or dna ro)
 (list ;; generic-+ generic-- generic-* generic-/
       generic:+ generic:- generic:* generic:/
       generic-= generic-< generic-> generic-<= generic->=
       generic-and generic-or generic-dna generic-ro))
 
(for-each
 (lambda (name underlying-operation)
   (assign-operation name
                     (v&s-unpacking underlying-operation)
                     v&s?))
 '(abs square sqrt generic-sign negate invert exp log not imp pmi identity)
 (list ;; generic-abs generic-square generic-sqrt
       g:abs g:square g:sqrt generic-sign g:negate g:invert g:exp g:log
       generic-not generic-imp generic-pmi generic-identity))

(assign-operation 'merge (coercing ->v&s v&s-merge) v&s? flat?)
(assign-operation 'merge (coercing ->v&s v&s-merge) flat? v&s?)
