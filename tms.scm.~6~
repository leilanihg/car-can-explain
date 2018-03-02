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

(declare (usual-integrations))

(define-structure
  (tms (type list)
       (named 'tms)
       (constructor %make-tms)
       (print-procedure #f))
  values)

(define (make-tms arg)
  (cond ((nothing? arg) (%make-tms (list arg)))
        ((contradiction? arg) (%make-tms (list arg)))
        ((v&s? arg) (%make-tms (list arg)))
        ((list? arg)
         (let ((args
                (filter (lambda (x)
                          (not (nothing? x)))
                        arg)))
           (if (every v&s? args)
               (if (null? args)
                   (%make-tms (list nothing))
                   (%make-tms args))
               (error "Bad make-tms" args))))
        (else (error "Bad make-tms" arg))))

(define-structure
  (hypothetical (type list)
		(named 'hypothetical)
		(print-procedure #f))
  amb-cell)

(define *premise-outness* (make-eq-hash-table))


(define (kick-out! premise)
  (if (premise-in? premise) (alert-all-propagators!))
  (mark-premise-out! premise))

(define (bring-in! premise)
  (if (not (premise-in? premise)) (alert-all-propagators!))
  (mark-premise-in! premise))

(define (premise-in? premise)
  (not (hash-table/get *premise-outness* premise #f)))

(define (mark-premise-in! premise)
  (hash-table/remove! *premise-outness* premise))

(define (mark-premise-out! premise)
  (hash-table/put! *premise-outness* premise #t))

(define (tms-merge tms1 tms2)
  (let ((candidate (tms-assimilate tms1 tms2)))
    (let ((consequence (strongest-consequence candidate)))
      ;;(check-consistent! consequence)
      (tms-assimilate candidate consequence))))

(assign-operation 'merge tms-merge tms? tms?)

(define (tms-assimilate tms stuff)
  (cond ((nothing? stuff) tms)
        ((v&s? stuff) (tms-assimilate-one tms stuff))
        ((tms? stuff)
         (fold-left tms-assimilate-one
                    tms
                    (tms-values stuff)))
        (else (error "This should never happen"))))

(define (tms-assimilate-one tms v&s)
  (if (any (lambda (old-v&s)
             (subsumes-v&s? old-v&s v&s))
           (tms-values tms))
      tms
      (let ((subsumed                   ;some v&s s.
             (filter (lambda (old-v&s)
                       (subsumes-v&s? v&s old-v&s))
                     (tms-values tms))))
        (make-tms
         (lset-adjoin equivalent-v&ss?
           (lset-difference equivalent-v&ss?
                            (tms-values tms)
                            subsumed)
           v&s)))))

(define (strongest-consequence tms)
  (let ((relevant-v&ss
         (filter all-premises-in?
                 (tms-values (->tms tms)))))
    (fold-left merge nothing relevant-v&ss)))

(define (all-premises-in? thing)
  (if (v&s? thing)
      (all-premises-in? (v&s-support thing))
      (every premise-in? thing)))

(define (equivalent-tmss? tms1 tms2)
  (or (eq? tms1 tms2)
      (and (tms? tms1) (tms? tms2)
           (lset= equivalent-v&ss?
                  (tms-values tms1)
                  (tms-values tms2)))))

#|
(define (check-consistent! v&s)
  (if (contradictory? v&s)
      (process-nogood! (v&s-support v&s)
		       (v&s-reasons v&s))))
|#

(define (tms-query tms)
  (let ((answer (strongest-consequence tms)))
    (let ((better-tms (tms-assimilate tms answer)))
      (if (not (eq? tms better-tms))
          (set-tms-values! tms (tms-values better-tms)))
      ;; (check-consistent! answer)
      answer)))

(assign-operation 'contradictory?
		  (lambda (tms)
		    (contradictory? (strongest-consequence tms)))
		  tms?)

(assign-operation 'nothingness?
		  (lambda (tms)
		    (nothingness? (strongest-consequence tms)))
		  tms?)

(define (tms-unpacking f)
  (lambda args
    (let ((relevant-information
	   (map (compose ->v&s tms-query ->tms) args)))
      (if (any nothing?
	       (map v&s-value relevant-information))
          nothing
          (make-tms (list (apply f relevant-information)))))))

(define (full-tms-unpacking f)
  (tms-unpacking (v&s-unpacking-and-coercing f)))

(define (->tms thing)
  (if (tms? thing)
      thing
      (make-tms (list (->v&s thing)))))

(for-each
 (lambda (name underlying-operation)
   (assign-operation
    name (full-tms-unpacking underlying-operation) tms? tms?)
   (assign-operation
    name (full-tms-unpacking underlying-operation) tms? v&s?)
   (assign-operation
    name (full-tms-unpacking underlying-operation) v&s? tms?)
   (assign-operation
    name (full-tms-unpacking underlying-operation) tms? flat?)
   (assign-operation
    name (full-tms-unpacking underlying-operation) flat? tms?))
 '(+ - * /
   generic-= generic-< generic-> generic-<= generic->=
   and or dna ro)
 (list generic:+ generic:- generic:* generic:/
       ;; generic-+ generic-- generic-* generic-/
       generic-= generic-< generic-> generic-<= generic->=
       generic-and generic-or generic-dna generic-ro))

(for-each
 (lambda (name underlying-operation)
   (assign-operation
    name (full-tms-unpacking underlying-operation) tms?))
 '(abs square sqrt generic-sign negate invert exp log not imp pmi identity)
 (list ;; generic-abs generic-square generic-sqrt
       g:abs g:square g:sqrt generic-sign g:negate g:invert g:exp g:log
       generic-not generic-imp generic-pmi generic-identity))

(assign-operation 'merge (coercing ->tms tms-merge) tms? v&s?)
(assign-operation 'merge (coercing ->tms tms-merge) v&s? tms?)
(assign-operation 'merge (coercing ->tms tms-merge) tms? flat?)
(assign-operation 'merge (coercing ->tms tms-merge) flat? tms?)


(define generic-ignore-first
  (make-generic-operator 2 'ignore-first ignore-first))

((lambda (name underlying-operation)
   (assign-operation
    name (v&s-unpacking underlying-operation) v&s? v&s?)
   (assign-operation
    name (coercing ->v&s underlying-operation) v&s? flat?)
   (assign-operation
    name (coercing ->v&s underlying-operation) flat? v&s?)

   (assign-operation
    name (full-tms-unpacking underlying-operation) tms? tms?)
   (assign-operation
    name (coercing ->tms underlying-operation) tms? v&s?)
   (assign-operation
    name (coercing ->tms underlying-operation) v&s? tms?)
   (assign-operation
    name (coercing ->tms underlying-operation) tms? flat?)
   (assign-operation
    name (coercing ->tms underlying-operation) flat? tms?))
 'ignore-first generic-ignore-first)


(assign-operation
 'true? (lambda (tms) (generic-true? (tms-query tms))) tms?)
