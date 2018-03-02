;;; ----------------------------------------------------------------------
;;; Copyright 2008 Alexey Radul and Gerald Jay Sussman.
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

(declare (usual-integrations make-cell))

(define generic-identity
  (make-generic-operator 1 'identity identity))

(define generic-+   (make-generic-operator 2 '+   +))
(define generic--   (make-generic-operator 2 '-   -))
(define generic-*   (make-generic-operator 2 '*   *))
(define generic-/   (make-generic-operator 2 '/   /))
(define generic-abs (make-generic-operator 1 'abs abs))
(define generic-sign (make-generic-operator 1 'sign sign-of-number))
(define generic-square (make-generic-operator 1 'square square))
(define generic-sqrt (make-generic-operator 1 'sqrt sqrt))

(define generic-=   (make-generic-operator 2 '=   default-equal?))
(define generic-<   (make-generic-operator 2 '<   <))
(define generic->   (make-generic-operator 2 '>   >))
(define generic-<=  (make-generic-operator 2 '<=  <=))
(define generic->=  (make-generic-operator 2 '>=  >=))

(define generic-sin (make-generic-operator 1 'sin sin))
(define generic-cos (make-generic-operator 1 'cos cos))
(define generic-tan (make-generic-operator 1 'tan tan))

(define generic-asin (make-generic-operator 1 'asin asin))
(define generic-acos (make-generic-operator 1 'acos acos))
(define generic-atan (make-generic-operator 1 'atan atan))

;; For explaining v&s values
(define generic-pp (make-generic-operator 1 'pp pp))

(define (generic-zero? x)
  (generic-= x 0))

(define generic-not (make-generic-operator 1 'not not))
(define generic-and (make-generic-operator 2 'and boolean/and))
(define generic-or  (make-generic-operator 2 'or  boolean/or))

;;; implication
(define (boolean/imp a) (if a #t nothing))
(define generic-imp (make-generic-operator 1 'imp boolean/imp))

;;; converse implication
(define (boolean/pmi a) (if (not a) #f nothing))
(define generic-pmi (make-generic-operator 1 'pmi boolean/pmi))

;;; DNA is to AND as division is to multiplication
(define (boolean/dna c x) (if (and (not c) x) #f nothing))
(define generic-dna (make-generic-operator 2 'dna boolean/dna))

;;; RO is to OR as division is to multiplication
(define (boolean/ro c x) (if (and c (not x)) #t nothing))
(define generic-ro (make-generic-operator 2 'ro boolean/ro))


(define (generic-extract-value x)
  (v&s-value (tms-query (->tms x))))
