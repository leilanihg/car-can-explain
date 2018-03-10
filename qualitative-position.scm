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

;;; This is a qualitative-position structure with 2 values: 
;;;   increasing ('right), decreasing ('dec), no change/zero ('0) and 
;;;   unknown change ('?)
;;; Currently only the following basic generic operations are supported:
;;; adding, subtracting, multiplication, division.

(define-structure
  (qualitative-position
   (type vector) (named 'qualitative-position) (print-procedure #f))
  lateral
  longitudinal)

(define qualitative-position-equal? equal?)

(define (pp-qualitative-position qual)
  (if (not (qualitative-position? qual)) 
      (display "cannot display value for non-qualitative-position value")
      (begin
	(cond ((isRight? qual) (display " positioned right"))
	      ((isLeft? qual) (display " positioned left"))
	      ((isNoneLateral? qual) 
	       (display " neutral lateral position"))
	      (else " unknown lateral position"))
	(cond ((isFront? qual) (display " and positioned front"))
	      ((isBack? qual) (display " and positioned back"))
	      ((isNoneLongitudal? qual) 
	       (display " and neutral longitudinal position"))
	      (else " and unknown longitudinal position")))))

(define (->qualitative-position x)
  (if (qualitative-position? x)
      x
      (make-qualitative-position x)))

(define (isUnknownLateral? qual)
  (if (eqv? (qualitative-position-lateral qual) '?) 
      #t 
      #f))

(define (isUnknownLongitudinal? qual)
  (if (eqv? (qualitative-position-longitudinal qual) '?) 
      #t 
      #f))

(define (isRight? qual)
  (if (eqv? (qualitative-direction-lateral qual) 'right) 
      #t 
      #f))

(define (isLeft? qual)
  (if (eqv? (qualitative-direction-lateral qual) 'left) 
      #t 
      #f))

(define (isNoneLateral? qual)
  (if (eqv? (qualitative-direction-lateral qual) '0) 
      #t 
      #f))

(define (isFront? qual)
  (if (eqv? (qualitative-direction-longitudinal qual) 'front)
      #t
      #f))

(define (isBack? qual)
  (if (eqv? (qualitative-direction-longitudinal qual) 'back)
      #t
      #f))


(define (isStraight? qual)
  (if (eqv? (qualitative-direction-lateral qual) '?) 
      #t 
      #f))

(define (isNoneLongitudinal? qual)
  (if (eqv? (qualitative-direction-longitudinal qual) '0) 
      #t 
      #f))

(define (add-single-position x y)
  (cond ((equal? x y) x)
	((or (equal? x '?) (equal? y '?)) '?) 
	((equal? x '0) y) 
	((equal? y '0) x) 
	(else '?)))

(define (add-qualitative-position x y)
  (let ((x-lat (qualitative-position-lateral x))
	(y-lat (qualitative-position-lateral y))
	(x-long (qualitative-position-longitudinal x))
	(y-long (qualitative-position-longitudinal y)))
    (make-qualitative-position (add-single-position x-lat y-lat)
			       (add-single-position x-long y-long))))

; This may not be the correct logic
(define (sub-qualitative-position x y)
  (add-qualitative-position x (negate-qualitative-position y)))

;; Very similar to addition
(define (mul-qualitative-position x y)
  (add-qualitative-position x y))

(define (negate-lateral x)
  (let ((lat (qualitative-position-lateral x)))
    (cond ((equal? lat 'right) 'left) 
	  ((equal? lat 'left) 'right)
	  (else '?))))

(define (negate-longitudinal x)
  (let ((long (qualitative-position-longitudinal x)))
    (cond ((equal? long 'front) 'back)
	  ((equal? long 'back) 'front)
	  (else '?))))

(define (negate-qualitative-position x)
  (make-qualitative-position (negate-lateral x)
			     (negate-longitudinal x)))

;; This may need to be changes
;; The logic is that a positive number corresponds to a positive
;; change, and a negative number corresponds to a decreasing change
;; If the number is 0, you return the qual...
(define (add-qualitative-position-number qual num)
  qual)

(define (multiply-qualitative-position-number qual num)
  qual)

; Adding brakes and gas to the position
; if increasing, add to the front (gas is the default)
(define (add-qualitative-position-qualitative qual-pos qual)
  (let ((lat (qualitative-position-lateral qual))
	(long (qualitative-position-longitudinal qual)))
    (cond ((isIncreasing? qual) 
	   (make-qualitative-position lat (add-single-position long
							       'back)))
	  ((isDecreasing? qual)
	   (make-qualitative-position lat (add-single-position long
							       'front)))
	  ((isZero? qual) qual-pos)
	  (else 
	   (make-qualitative-position lat (add-single position long '?))))))

(define (add-qualitative-position-direction qual-pos qual-dir)
  (let ((lat (qualitative-position-lateral qual))
	(long (qualitative-position-longitudinal qual)))
    (cond ((isRight? qual-pos) 
	   (make-qualitative-position (add-single-position lat 'left)
				      long))
	  ((isLeft? qual-pos)
	   (make-qualitative-position (add-single-position lat 'right)
				      long))
	  ((isZero? qual) qual-pos)
	  (else 
	   (make-qualitative-position (add-single position lat '?) long)))))

(assign-operation '+ add-qualitative-position qualitative-position? qualitative-position?)
(assign-operation '+ add-qualitative-position-number qualitative-position? number?)
(assign-operation '+ (reverse-args add-qualitative-position-number) number? qualitative-position?)
(assign-operation '+ add-qualitative-position-qualitative
		  qualitative-position? qualitative?)
(assign-operation '+ (reverse-args add-qualitative-position-qualitative)
		  qualitative? qualitative-position?)
(assign-operation '+ add-qualitative-position-direction
		  qualitative-position? qualitative-direction?)
(assign-operation '+ (reverse-args add-qualitative-position-direction)
		  qualitative-direction? qualitative-position?)

(assign-operation '- sub-qualitative-position qualitative-position? qualitative-position?)
(assign-operation '- (coercing ->qualitative-position sub-qualitative-position) number? qualitative-position?)
(assign-operation '- (coercing ->qualitative-position sub-qualitative-position) qualitative-position? number?)

(assign-operation '* mul-qualitative-position qualitative-position? qualitative-position?)
(assign-operation '* multiply-qualitative-position-number number? qualitative-position?)
(assign-operation '* (reverse-args multiply-qualitative-position-number) qualitative-position? number?)
(assign-operation 'not negate-qualitative-position qualitative-position?)

;(assign-operation 'pp pp-qualitative-position qualitative-position?)

;; Right now, this is just an add
(define (merge-qualitative-positions content increment)
  (add-qualitative-position content increment))

(define (merge-qualitative-position-number qual number)
  number)

(assign-operation 'merge merge-qualitative-positions qualitative-position? qualitative-position?)
(assign-operation 'merge merge-qualitative-position-number qualitative-position? number?)
(assign-operation 'merge (reverse-args merge-qualitative-position-number) 
		  number? qualitative-position?)

;; I think qualtitative description is defined
(define (equivalent-qualitative-positions? i1 i2)
  (eqv? (qualitative-position-description il)
		       (qualitative-position-description i2)))

;(assign-operation 'equivalent? equivalent-qualitative-positions? qualitative-position? qualitative-position?)
;(assign-operation 'equivalent? (coercing ->qualitative-position equivalent-qualitative-positions?) 
;		  number? qualitative-position?)
;(assign-operation 'equivalent? (coercing ->qualitative-position equivalent-qualitative-positions?) 
;		  qualitative-position? number?)
