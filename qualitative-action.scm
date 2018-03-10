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

;;; code that is necessary to run examples,
(define-structure
  (qualitative-action
   (type vector) (named 'qualitative-action) (print-procedure #f))
  description)

(define qualitative-action-equal? equal?)

; might want to describe what empty is here

(define (->qualitative-action x)
  (if (qualitative-action? x)
      x
      (make-qualitative-action x)))

(define (add-qualitative-action x y)
  (let ((x-description (qualitative-action-description x))
	(y-description (qualitative-action-description y)))
    (cond ((equal? x-description y-description)
	   (make-qualitative-action x-description))
	  ((or (equal? x-description 'unknown) 
	       (equal? y-description 'unknown-action)) 
	   (make-qualitative-action 'unknown-action))
	  ((equal? x-description 'nothing) 
	   (make-qualitative-action y-description))
	  ((equal? y-description 'nothing) 
	   (make-qualitative-action x-description))
	  (else (make-qualitative-action 'unknown-action)))))

; This may not be the correct logic
(define (sub-qualitative-action x y)
  (add-qualitative-action x (negate-qualitative-action y)))

;; Very similar to addition
; tighten loosen same unknown-action
(define (mul-qualitative-action x y)
  (add-qualitative-action x y))

(define (negate-qualitative-action x)
  (let ((desc (qualitative-action-description x)))
    (cond ((equal? desc 'tighten) (make-qualitative-action 'loosen))
	  ((equal? desc 'loosen) (make-qualitative-action 'tighten))
	  (else (make-qualitative-action 'unknown-action))))) 

;; This may need to be changes
;; The logic is that a positive number corresponds to a positive
;; change, and a negative number corresponds to a decreasing change
;; If the number is 0, you return the qual...
(define (merge-qualitative-action-number action num)
  (cond ((> num 0)
	 (add-qualitative-action action (make-qualitative-action 'tighten)))
	((eqv? 0 num) action)
	(else
	 (add-qualitative-action action (make-qualitative-action
				       'loosen)))))

(define (merge-action-qualitative action qualitative)
  (cond ((> num 0)
	 (add-qualitative-action action (make-qualitative-action 'tighten)))
	((eqv? 0 num) action)
	(else
	 (add-qualitative-action action (make-qualitative-action 'loosen)))))


(assign-operation '+ add-qualitative-action 
		  qualitative-action? qualitative-action?)
(assign-operation '+ merge-qualitative-action-number ; merge-qualitatives
		  qualitative-action? qualitative?)
(assign-operation '+ merge-qualitative-action-number ; merge-qualitatives 
		  qualitative? qualitative-action?)
(assign-operation '+ merge-qualitative-action-number 
		  qualitative-action? number?)
(assign-operation '+ (reverse-args merge-qualitative-action-number) 
		  number? qualitative-action?)

(assign-operation '- sub-qualitative-action 
		  qualitative-action? qualitative-action?)
(assign-operation '- (coercing ->qualitative-action
			       sub-qualitative-action) 
		  number? qualitative-action?)
(assign-operation '- (coercing ->qualitative-action
			       sub-qualitative-action) 
		  qualitative-action? number?)

(assign-operation '* mul-qualitative-action 
		  qualitative-action? qualitative-action?)
(assign-operation '* (coercing ->qualitative-action
			       mul-qualitative-action)
		  number? qualitative-action?)
(assign-operation '* (coercing ->qualitative-action
			       mul-qualitative-action) 
		  qualitative-action? number?)
(assign-operation 'not negate-qualitative-action qualitative-action?)
;; I think we need an identity one here

;; Four actions right now, more may need to be added
(define (to-qualitative-from-action action-object)
  (let ((action-description (qualitative-action-description action-object)))
    (cond ((equal? action-description 'tighten) (make-qualitative 'inc))
	  ((equal? action-description 'loosen) (make-qualitative 'dec))
	  ((equal? action-description 'no-action) (make-qualitative '0))
	  (else (make-qualitative '?))))) 

;; Four actions right now, more may need to be added
(define (to-action-from-qualitative qual-object)
  (let ((qual-description (qualitative-description qual-object)))
    (cond ((equal? qual-description 'inc) (make-qualitative-action 'tighten))
	  ((equal? qual-description 'desc) (make-qualitative-action 'loosen))
	  ((equal? qual-description '0) (make-qualitative-action 'no-action))
	  (else (make-qualitative-action 'unknown-action))))) 

;; In the future, this may need to be more sophisciated
;; Currently - this is just an add
(define (merge-qualitative-actions content increment)
  (add-qualitative-action content increment))

(define (merge-action-and-qualtitative action qualitative)
  (add-qualitative action action (to-action-from-qualitative qualitative)))


;; I think qualtitative description is defined
(define (equivalent-qualitative-actions? i1 i2)
  (eqv? (qualitative-action-description il)
		       (qualitative-action-description i2)))

;; I think qualtitative description is defined
(define (equivalent-action-and-qualitative action qualitative)
  (eqv? (qualitative-action-description action)
	(qualitative-action-description 
	 (to-action-from-qualitative qualitative))))

(assign-operation 'merge merge-qualitative-actions 
		  qualitative-action? qualitative-action?)
(assign-operation 'merge merge-qualitative-action-number 
		  qualitative-action? number?)
(assign-operation 'merge (reverse-args merge-qualitative-action-number) 
		  number? qualitative-action?)
(assign-operation 'merge merge-action-qualitative 
		  qualitative-action? qualitative?)
(assign-operation 'merge (reverse-args merge-action-qualitative) 
		  qualitative? qualitative-action?)


;(assign-operation 'equivalent? equivalent-qualitative-actions? 
;		  qualitative-action? qualitative-action?)
;(assign-operation 'equivalent? (coercing ->qualitative-action 
;					 equivalent-qualitative-actions?) 
;		  number? qualitative-action?)
;(assign-operation 'equivalent? (coercing ->qualitative-action 
;					 equivalent-qualitative-actions?) 
;		  qualitative-action? number?)
;(assign-operation 'equivalent? equivalent-action-and-qualitative
;		  qualitative-action? qualitative?)
;(assign-operation 'equivalent? (reverse-args equivalent-action-and-qualitative)
;		  qualitative? qualitative-action?)


