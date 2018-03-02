;;; ----------------------------------------------------------------------
;;; Copyright 2016 Leilani H Gilpin, Ben Z Yuan and Gerald Jay Sussuman 
;;; ----------------------------------------------------------------------
;;; This file is part of Artistic Propagator Prototype.
;;; 
;;; Artistic Propagator Prototype is free software; you can
;;; redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software
;;; Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;; 
;;; Artistic Propagator Prototype is distributed in the hope that it
;;; will be useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Artistic Propagator Prototype.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

;;; This is a qualitative structure with 4 description values: 
;;;   increasing ('inc), decreasing ('dec), no change/zero ('0) and 
;;;   unknown change ('?)
;;;  And 4 direction values:
;;;   right ('right), left ('left), straight-ahead ('0) and unknown ('?)
;;; Currently only the following basic generic operations are supported:
;;; adding, subtracting, multiplication, division.

(define-structure
  (qualitative-direction
   (type vector) (named 'qualitative-direction) (print-procedure #f))
  description
  turn)

(define qualitative-direction-equal? equal?)

(define (pp-qualitative-direction qual)
  (if (not (qualitative-direction? qual)) 
      (display "cannot display value for non-qualitative-direction value")
      (begin
	(cond ((isUnknown-desc? qual) (display "unknown change"))
	      ((isIncreasing? qual) (display "increasing change"))
	      ((isDecreasing? qual) (display "decreasing change"))
	      (else "no change"))
	(cond ((isRight? qual) (display " turning right"))
	      ((isLeft? qual) (display " turning left"))
	      ((isStraight? qual) 
	       (display " no turn: pointed straight ahead"))
	      (else " unknown direction")))))

(define (->qualitative-direction x)
  (if (qualitative-direction? x)
      x
      (make-qualitative-direction x)))

(define (isUnknown-desc? qual)
  (if (eqv? (qualitative-direction-description qual) '?) 
      #t 
      #f))

(define (isUnknown-turn? qual)
  (if (eqv? (qualitative-direction-turn qual) '?) 
      #t 
      #f))

(define (isIncreasing? qual)
  (if (eqv? (qualitative-direction-description qual) 'inc) 
      #t 
      #f))

(define (isDecreasing? qual)
  (if (eqv? (qualitative-direction-description qual) 'dec) 
      #t 
      #f))

(define (isZero-desc? qual)
  (if (eqv? (qualitative-direction-description qual) '0) 
      #t 
      #f))

(define (isRight? qual)
  (if (eqv? (qualitative-direction-turn qual) 'right) 
      #t 
      #f))

(define (isLeft? qual)
  (if (eqv? (qualitative-direction-turn qual) 'left) 
      #t 
      #f))

(define (isStraight? qual)
  (if (eqv? (qualitative-direction-turn qual) '0) 
      #t 
      #f))

(define (isStraight? qual)
  (if (eqv? (qualitative-direction-turn qual) '?) 
      #t 
      #f))

(define (add-qualitative-direction x y)
  (let ((x-desc (qualitative-direction-description x))
	(y-desc (qualitative-direction-description y))
	(x-turn (qualitative-direction-turn x))
	(y-turn (qualitative-direction-turn y)))
    (let ((added-desc (qualitative-description
		       (generic-+ (make-qualitative x-desc)
				  (make-qualitative y-desc)))))
      (cond ((equal? x-turn y-turn)
	     (make-qualitative-direction added-desc x-turn))
	    ((or (equal? x-turn '?) (equal? y-turn '?)) 
	     (make-qualitative-direction added-desc '?))
	    ((equal? x-turn '0) 
	     (make-qualitative-direction added-desc y-turn))
	    ((equal? y-turn '0) 
	     (make-qualitative-direction added-desc x-turn))
	    (else (make-qualitative-direction added-desc '?))))))

; This may not be the correct logic
(define (sub-qualitative-direction x y)
  (add-qualitative-direction x (negate-qualitative-direction y)))

;; Very similar to addition
(define (mul-qualitative-direction x y)
  (add-qualitative-direction x y))

; LG - I don't think we will need this
#|
(define (empty-qualitative-direction? x)
  (if (
  (> (qualitative-direction-low x) (qualitative-direction-high x)))
|#

;;; Might need some type of identity function

(define (negate-qualitative-direction x)
  (let ((negated-desc (qualitative-description
		       (generic-not (make-qualitative x-desc)
				    (make-qualitative y-desc)))))
    (let ((x-turn (qualitative-direction-turn x)))
      (cond ((equal? desc 'right) 
	     (make-qualitative-direction negated-desc 'left)) 
	    ((equal? desc 'left) 
	     (make-qualitative-direction negated-desc 'right))
	    (else (make-qualitative-direction negated-desc '?))))))

;; This may need to be changes
;; The logic is that a positive number corresponds to a positive
;; change, and a negative number corresponds to a decreasing change
;; If the number is 0, you return the qual...
(define (merge-qualitative-direction-number qual num)
  (cond ((> num 0)
	 (add-qualitative-direction qual (make-qualitative-direction
					  '0 'inc)))
	((eqv? 0 num) qual)
	(else
	 (add-qualitative-direction qual (make-qualitative-direction 
					  '0 'dec)))))

(define (merge-qualitative-direction-qualitative qual-direction qual)
  (let ((desc (qualitative-description qual)))
    (add-qualitative-direction qual-direction
			       (make-qualitative-direction '0 desc))))


(assign-operation '+ add-qualitative-direction 
		  qualitative-direction? qualitative-direction?)
(assign-operation '+ merge-qualitative-direction-number 
		  qualitative-direction? number?)
(assign-operation '+ (reverse-args merge-qualitative-direction-number) 
		  number? qualitative-direction?)
(assign-operation '+ merge-qualitative-direction-qualitative 
		  qualitative-direction? qualitative?)
(assign-operation '+ (reverse-args 
		      merge-qualitative-direction-qualitative) 
		  qualitative? qualitative-direction?)


(assign-operation '- sub-qualitative-direction qualitative-direction? qualitative-direction?)
(assign-operation '- (coercing ->qualitative-direction sub-qualitative-direction) number? qualitative-direction?)
(assign-operation '- (coercing ->qualitative-direction sub-qualitative-direction) qualitative-direction? number?)

(assign-operation '* mul-qualitative-direction qualitative-direction? qualitative-direction?)
(assign-operation '* (coercing ->qualitative-direction mul-qualitative-direction) number? qualitative-direction?)
(assign-operation '* (coercing ->qualitative-direction mul-qualitative-direction) qualitative-direction? number?)
(assign-operation 'not negate-qualitative-direction qualitative-direction?)

(assign-operation 'pp pp-qualitative-direction qualitative-direction?)

;; Right now, this is just an add
(define (merge-qualitative-directions content increment)
  (add-qualitative-direction content increment))

(assign-operation 'merge merge-qualitative-directions qualitative-direction? qualitative-direction?)
(assign-operation 'merge merge-qualitative-direction-number qualitative-direction? number?)
(assign-operation 'merge (reverse-args merge-qualitative-direction-number) 
		  number? qualitative-direction?)

;; I think qualtitative description is defined
(define (equivalent-qualitative-directions? i1 i2)
  (eqv? (qualitative-direction-description il)
		       (qualitative-direction-description i2)))

(assign-operation 'equivalent? equivalent-qualitative-directions? qualitative-direction? qualitative-direction?)
(assign-operation 'equivalent? (coercing ->qualitative-direction equivalent-qualitative-directions?) 
		  number? qualitative-direction?)
(assign-operation 'equivalent? (coercing ->qualitative-direction equivalent-qualitative-directions?) 
		  qualitative-direction? number?)