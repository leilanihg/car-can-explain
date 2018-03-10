;;; ----------------------------------------------------------------------
;;; Copyright 2008--2016 Alexey Radul and Gerald Jay Sussman
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

(define generic-identity
  (make-generic-operator 1 'identity identity))

#| ;;; Use scmutils g:+, g:-, etc
(define generic-+   (make-generic-operator 2 '+   +))
(define generic--   (make-generic-operator 2 '-   -))
(define generic-*   (make-generic-operator 2 '*   *))
(define generic-/   (make-generic-operator 2 '/   /))
(define generic-abs (make-generic-operator 1 'abs abs))
|#

(define generic-sign
  (make-generic-operator 1 'generic-sign sign-of-number))

#| ;;; Use scmutils
(define generic-square (make-generic-operator 1 'square square))
(define generic-sqrt (make-generic-operator 1 'sqrt sqrt))
|#

(define generic-=   (make-generic-operator 2 'generic-=   default-equal?))
(define generic-<   (make-generic-operator 2 'generic-<   <))
(define generic->   (make-generic-operator 2 'generic->   >))
(define generic-<=  (make-generic-operator 2 'generic-<=  <=))
(define generic->=  (make-generic-operator 2 'generic->=  >=))

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
  (let ((v (tms-query (->tms x))))
    (cond ((nothingness? v) v)
	  ((v&s? v) (v&s-value v))
	  (else (error "What is this?" x)))))
