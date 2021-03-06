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

;;; Constructors for primitive propagators are constructed here!

(define (constant value)
  (function->propagator-constructor
   (lambda () value)
   `(constant ,value)))

(define copier
  (function->propagator-constructor generic-identity 'identity))

#|
(define adder (function->propagator-constructor generic-+))
(define subtractor (function->propagator-constructor generic--))
(define multiplier
  (function->propagator-constructor generic-* '* apply-f-multiply))
(define divider
  (function->propagator-constructor generic-/ '/ apply-f-divide))
|#

(define adder (function->propagator-constructor g:+ '+))
(define subtractor (function->propagator-constructor g:- '-))
(define multiplier
  (function->propagator-constructor g:* '* apply-f-multiply))
(define divider
  (function->propagator-constructor g:/ '/ apply-f-divide))

(define p:+ adder)
(define p:- subtractor)
(define p:* multiplier)
(define p:/ divider)

#|
(define absolute-value (function->propagator-constructor generic-abs))
|#

(define absolute-value (function->propagator-constructor g:abs))
(define p:abs absolute-value)

(define sign (function->propagator-constructor generic-sign))
(define p:sign sign)

#|
(define p:square (function->propagator-constructor generic-square))
(define p:sqrt (function->propagator-constructor generic-sqrt))
|#

(define squarer (function->propagator-constructor g:square))
(define sqrter (function->propagator-constructor g:sqrt))
(define p:negate (function->propagator-constructor g:negate))
(define p:invert (function->propagator-constructor g:invert))

(define p:square squarer)
(define p:sqrt sqrter)

(define p:exp (function->propagator-constructor g:exp))
(define p:log (function->propagator-constructor g:log))

(define =? (function->propagator-constructor generic-=))
(define <? (function->propagator-constructor generic-<))
(define >? (function->propagator-constructor generic->))
(define <=? (function->propagator-constructor generic-<=))
(define >=? (function->propagator-constructor generic->=))

(define inverter (function->propagator-constructor generic-not))
(define conjoiner-dumb (function->propagator-constructor generic-and))
(define disjoiner-dumb (function->propagator-constructor generic-or))

(define p:inverter inverter)

;;; Used for logical short cuts
(define p:imp (function->propagator-constructor generic-imp))
(define p:pmi (function->propagator-constructor generic-pmi))

;;; DNA is to AND as division is to multiplication
(define p:dna (function->propagator-constructor generic-dna))

;;; RO is to OR as division is to multiplication
(define p:ro (function->propagator-constructor generic-ro))

;;; Short cut if any input is false

(define (conjoiner a b c)
  (define me
    (propagator (list a b) c
      (lambda ()
        (conjoiner-dumb a b c)
        (p:pmi a c)
        (p:pmi b c))
      `(conjoiner (,(name a) ,(name b)) ,(name c))))
  me)
    

;;; Short cut if any input is true

(define (disjoiner a b c)
  (define me
    (propagator (list a b) c
      (lambda ()
        (disjoiner-dumb a b c)
        (p:imp a c)
        (p:imp b c))
      `(disjoiner (,(name a) ,(name b)) ,(name c))))
  me)  

(define (conditional p if-true if-false output)
  (define me
    (propagator (list p if-true if-false) output
      (lambda ()
        (let ((predicate (content p)))
          (if (nothing? predicate)
              'done
              (add-content
               output
               (if (generic-true? predicate)
                   (generic-ignore-first predicate (content if-true))
                   (generic-ignore-first predicate (content if-false)))
               me))))
      `(conditional ,(name p)
                    ,(name if-true) ,(name if-false)
                    ,(name output))))
  me)

(define spdt-switch conditional)

(define (switch predicate if-true output)
  (conditional predicate if-true (make-cell 'empty-cell) output))

(define (spst-switch p if-true output)
  (define me
    (propagator (list p if-true) output
      (lambda ()
        (let ((predicate (content p)))
          (if (nothing? predicate)
              'done
               (if (generic-true? predicate)
                   (add-content output
                     (generic-ignore-first predicate
                                           (content if-true))
                     me)))))
      `(spst-switch ,(name p)
                    ,(name if-true)
                    ,(name output))))
  me)


(define true? (lambda (x) (not (not x))))

(define generic-true? (make-generic-operator 1 'true? true?))



