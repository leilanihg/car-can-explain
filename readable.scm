;;; ----------------------------------------------------------------------
;;; Copyright 2016 Leilani H Gilpin, Ben Z Yuan and Gerald Jay Sussman
;;; ----------------------------------------------------------------------
;;; This file is part of Propagator Network Prototype.
;;;
;;; Propagator Network Prototype is free software; you can
;;; redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software
;;; Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; Propagator Network Prototype is distributed in the hope that it
;;; will be useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Propagator Network Prototype.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

;;; Some nicer, human readable user interface code, to be improved.

(declare (usual-integrations make-cell cell?))

;;; This removes those annoying hash numbers after ;Value:
(set! repl:write-result-hash-numbers? #f)

;;; Make for nice transcripts.
(define (cpp x)
  (display "#;\n")
  (pp x))


;; Mapping from cell-name to readable name
(define (display-cell-name cell)
  (if (not (cell? cell))
      (error "not a cell") ; throw an error
      (display (nickname cell))))
#|
      (let ((short-name (nickname cell))
	    (long-name (name cell)))
	(if (null? short-name)
	    (if (list? short-name)
		(display (car short-name))
		(display short-name))
	    (if (list? name)
		(display (car name))
		(display name))))))
|#
; Takes a v&s and makes it human readable
;
(define (reasons-readable v&s-prop)
  (display v&s-prop)
)

(define (display-antecedent-structures v&s)
  (map generic-pp (v&s-reasons v&s))
  (display "\n")
)

(define (display-support v&s)
  (display (v&s-support v&s))
  (display "\n")
)

;;; This is required because (run) returns old value if there is
;;; nothing to do.  This is a problem if a contradiction is resolved
;;; by a kick-out! with no propagation.

(define (inquire-readable cell)
  (assert (cell? cell) "Can only inquire of a cell.")
  (let ((v (run)))
    (if (not (eq? v 'done)) (write-line v)))
  (display-value-readable cell (tms-query (->tms (content cell)))))

(define (display-value-readable cell v&s)
  (if (nothing? v&s)
      ;; the cell has a *the-nothing*, which is unexplainable
      (begin (display (nickname cell))
	     (display " has an indeterminate value."))
      ;; otherwise the cell has something we can explain
      (begin (display-cell-name  cell)
	     (display " has the value ")
	     (generic-pp (v&s-value v&s))
	     (display "\n\n")
	     (display "This was computed from the following: \n\n")
	     (display-antecedent-structures v&s)
	     (if (not (null? (v&s-support v&s)))
         (begin (display "This value is supported by the ")
  	     (display "following premises:\n\n")
  	     (display-support v&s)
         (display "\n"))
         #f)
       (display "----------------------------------------\n\n"))))

#|
(define (show-value-readable cell v&s)
  (if (nothing? v&s)
      `(,(nickname cell) (has-value ,v&s))
      `(,(nickname cell)
        (has-value ,(v&s-value v&s))
        (because ,@(map name (v&s-reasons v&s)))
        (depends-on ,@(map nickname (v&s-support v&s))))))
|#

; Might have to have something for *the-nothing*
(define (explain-readable cell)
  (assert (cell? cell) "Can only explain a cell.")
  (let ((mark (make-eq-hash-table)))
    (define (walk cell)
      (let ((seen (hash-table/get mark cell #f)))
	(if (not seen)
            (let ((val (tms-query (->tms (content cell)))))
              (hash-table/put! mark cell #t)
              (cons (display-value-readable cell val)
                    (append-map
                     (lambda (reason)
                       (if (eq? reason *universal-ancestor*)
                           '()
			   ; Some reasons are not propagator
			   (if (propagator? reason)
			       (append-map walk
					   (propagator-inputs reason))
			       (generic-pp reason))))
                     (v&s-reasons val))))
            '())))
    (walk cell)))
