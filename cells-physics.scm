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

;;; ----------------------------------------------------------------------
;;; This file contains all the functions that are necessary to run the 
;;; propogator constructs, but it does not contain all the code to run
;;; the examples in the paper.  
;;; ----------------------------------------------------------------------

(declare (usual-integrations make-cell))

;;;  A cell is represented by a message-acceptor.
;;;  A cell has a parent, of which it is a part.
;;;  There is a top-level parent of the sytem.  The fluid
;;;  variable *my-parent* is defined in scheduler.scm

(define (make-cell #!optional name)
  (let ((neighbors '()) (content nothing) (informants '()))
    (define (new-neighbor! new-neighbor)
      (if (not (memq new-neighbor neighbors))
          (begin (set! neighbors (cons new-neighbor neighbors))
                 (alert-propagators new-neighbor))))
    (define (add-content increment source)  ; ***
      (let ((answer (merge content increment)))
	(if add-content-wallp
	    (write-line `(cell-assigned
			  ,name ,content ,increment ,answer)))
	(cond ((equivalent? answer content) 'ok)
	      ((equivalent? answer increment)
	       (set! content increment)
	       (set! informants (list source))
	       (alert-propagators neighbors))
	      ((contradictory? answer)
	       (error "Ack! Inconsistency!"
		      (me 'name)
		      answer
		      content))
	      (else (set! content answer)
		    (set! informants
			  (lset-adjoin eq?
				       informants source))
		    (alert-propagators neighbors)))))
    (define (me message)
      (case message
        ((add-content) add-content)
        ((content) content)
        ((new-neighbor!) new-neighbor!)
	((neighbors) neighbors)
        ((informants) informants)
        ((name) name)
        ((set-name!)
         (lambda (new)
           (set! name new)
           (eq-put! me 'name name)))
        (else
	 (error "Unknown message" message)
	 me)))
    (if (default-object? name)
	((me 'set-name!) me)
	((me 'set-name!) name)) 
    (eq-put! me 'cell #t)               ;I am a cell!
    (eq-put! me 'parent *my-parent*)
    (eq-adjoin! *my-parent* 'children me)
    me))

(define add-content-wallp #f)

(define (cell? thing)
  (eq-get thing 'cell))

(define (cell-name cell)
  (cell 'name))

(define (set-cell-name! cell name)
  ((cell 'set-name!) name))

(define (new-neighbor! cell neighbor)
  ((cell 'new-neighbor!) neighbor))

(define (neighbors cell)
  (cell 'neighbors))

(define (add-content cell increment #!optional source)
  (if (default-object? source) (set! source *my-parent*))
  ((cell 'add-content) increment source))

(define (content cell)
  (cell 'content))

(define (informants cell)
  (cell 'informants))

(define nothing
  (list '*the-nothing*))

(define (nothing? thing)
  (eq? thing nothing))

(define the-contradiction
  (list 'contradiction))



(define merge
  (make-generic-operator 2 'merge
   (lambda (content increment)
     (if (default-equal? content increment)
         content
         the-contradiction))))

(assign-operation 'merge
 (lambda (content increment) content)
 any? nothing?)

(assign-operation 'merge
 (lambda (content increment) increment)
 nothing? any?)


(define equivalent?
  (make-generic-operator 2
			 'equivalent?
			 default-equal?))
  

(define contradictory?
  (make-generic-operator 1 'contradictory?
   (lambda (thing)
     (eq? thing the-contradiction))))
