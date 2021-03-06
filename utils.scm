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

(declare (usual-integrations make-cell cell?))

(define (for-each-distinct-pair proc lst)
  (if (not (null? lst))
      (let loop ((first (car lst)) (rest (cdr lst)))
	(for-each (lambda (other-element)
		    (proc first other-element))
		  rest)
	(if (not (null? rest))
	    (loop (car rest) (cdr rest))))))

(define (sort-by lst compute-key)
  (map cdr
       (sort (map (lambda (thing)
		    (cons (compute-key thing) thing))
		  lst)
	     (lambda (pair1 pair2)
	       (< (car pair1) (car pair2))))))

(define (listify object)
  (cond ((null? object) object)
	((pair? object) object)
	(else (list object))))

(define (lset< = s1 s2)
  (and (lset<= = s1 s2) (not (lset= = s1 s2))))

(define (identity x) x)

(define (ignore-first x y) y)

(define (reverse-args f)
  (lambda (x y)
    (f y x)))

(define (compose . fs)
  (define ((compose2 f g) x)
    (f (g x)))
  (define (composen fs)
    (cond ((null? fs) identity)
	  ((null? (cdr fs)) (car fs))
	  (else
	   (compose2 (car fs)
		     (composen (cdr fs))))))
  (composen fs))

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

(define (make-counter #!optional name)
  (let ((count 0))
    (define (the-counter)
      (set! count (+ count 1))
      count)
    the-counter))

(define (make-new-symbols name)
  (let ((count 0))
    (define (the-generator)
      (set! count (+ count 1))
      (symbol name count))
    the-generator))

(define (hash-table-intern! table key get-value)
  (or (hash-table-ref/default table key #f)
      (let ((value (get-value)))
        (hash-table-set! table key value)
        value)))

;;; Needed for SRFI 69:
(define hash
  (access hash (->environment '(runtime hash-table))))

(define (hash-by-eqv object #!optional modulus)
  (if (default-object? modulus)
      (eqv-hash object)
      (eqv-hash-mod object modulus)))

(define (any? x) #t)

(define (numeric? x)
  (or (number? x) (abstract-number? x)))

(define *equality-tolerance* 1e-15)

(define (default-equal? x y #!optional tolerance)
  (or (eq? x y)
      (if (and (number? x) (number? y))
	  (if (and (exact? x) (exact? y))
	      (= x y)
	      (close-enuf? x y
			   (if (default-object? tolerance)
			       *equality-tolerance*
			       tolerance)))
	  (equal? x y))))

(define (close-enuf? h1 h2 #!optional tolerance scale)
  (if (default-object? tolerance)
      (set! tolerance *machine-epsilon*))
  (if (default-object? scale)
      (set! scale 1.0))
  (<= (magnitude (- h1 h2))
      (* tolerance
         (+ (* 0.5
	       (+ (magnitude h1) (magnitude h2)))
	    scale))))

(define *machine-epsilon*
  (let loop ((e 1.0))
     (if (= 1.0 (+ e 1.0))
         (* 2 e)
         (loop (/ e 2)))))

(define (sign-of-number n)
  (if (not (number? n))
      (error "sign: Not a number" n))
  (cond ((< n 0) -1)
        ((= n 0) nothing)
        ((> n 0) +1)))

(define (pairwise-union nogoods1 nogoods2)
  (apply
   append
   (map (lambda (nogood1)
	  (map (lambda (nogood2)
		 (lset-union eq? nogood1 nogood2))
	       nogoods2))
	nogoods1)))

(define (coercing coercer f)
  (lambda args
    (apply f (map coercer args))))

;;;    Names of things
;;; Should also be a way to get the name of a generic 
;;; operator or an MIT/GNU primitive -- will figure this
;;; out later.

(define (name thing)
  (if (eq? thing *universal-ancestor*)
      ;;(list *universal-ancestor*)
      '()
      (let ((p (parent thing)))
        (if p
            (let ((n (name p)))
              (cons (given-name thing)
                    n))
            (list (given-name thing))))))

(define (given-name thing)
  (cond ((or (cell? thing) (propagator? thing) (physob? thing))
         (thing 'name))
        ((eq-get thing 'name))
        (else thing)))

(define (name! thing name)
  (cond ((or (cell? thing) (propagator? thing) (physob? thing))
         ((thing 'set-name!) name))
        (else
         (eq-put! thing 'name name))))

(define (parent thing)
  (eq-get thing 'parent))


(define (children thing)
  (or (eq-get thing 'children)
      '()))

(define (thing path)
  (cond ((null? path)
	 *universal-ancestor*)
	((equal? (car path) *universal-ancestor*)
	 *universal-ancestor*)
	(else
	 (find (lambda (kid)
		 (equal? (given-name kid) (car path)))
	       (children (thing (cdr path)))))))

;;; NICKNAME produces a name that cannot be inverted by thing, because
;;; it is not unique, but it is short and reasonable to read.

(define (nickname thing)
  (define (short given)
    (if (list? given)
	(if (eq? (car given) 'function-propagator)
	    (caadr given)
	    (car given))
	given))
  (if (eq? thing *universal-ancestor*)
      '()
      (let ((p (parent thing)))
        (if p
            (let ((surname (nickname p))
		  (given (given-name thing)))
	      (cons (short given) surname))
            (list (short (given-name thing)))))))

