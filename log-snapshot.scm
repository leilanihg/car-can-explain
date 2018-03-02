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

;; This is the definition for the log snapshot structure
;; This structure keeps a record of all the can-bus records for a
;; particular time.  

(define-structure
  (log-snapshot
   (type vector) (named 'log-snapshot) (print-procedure #f))
  time lateral accel steer brake gear-rotate front-wheel back-wheel 
  speed drive-mode gas-pedal ground-truth heading)

(define snapshot-length 13)

(define log-snapshot-equal? equal?)

(define (->log-snapshot x)
  (if (log-snapshot? x)
      x
      (make-log-snapshot '0 '0 '0 '0 '0 '0 '0 '0 '0 '0
			 '0 '0 '0 '0 '0 '0 '0 '0 '0 '0
			 '0 '0 '0 '0)))

; Keeps a record of the bus-codes and their value
(define *bus-code-mapping*)

;; In the future, the bus code mapping may be described in a file (for
;; each vehicle-schema).  
;; For now, it is hard-coded here.
(define (make-bus-code-mapping #!optional mapping-file)
  (set! *bus-code-mapping* (make-strong-eq-hash-table))
  (hash-table/put! *bus-code-mapping* 22 'lateral)
  (hash-table/put! *bus-code-mapping* 23 'acceleration)
  (hash-table/put! *bus-code-mapping* 25 'steering)
  (hash-table/put! *bus-code-mapping* 30 'brakes)
  (hash-table/put! *bus-code-mapping* '3e 'gear-rotation)
  (hash-table/put! *bus-code-mapping* 'b1 'front-wheels)
  (hash-table/put! *bus-code-mapping* 'b3 'back-wheels)
  (hash-table/put! *bus-code-mapping* 'b4 'speed)
  (hash-table/put! *bus-code-mapping* 120 'drive-mode)
  (hash-table/put! *bus-code-mapping* 244 'gas-pedal)
  (hash-table/put! *bus-code-mapping* 'groundtruthxyz 'ground-truth)
  (hash-table/put! *bus-code-mapping* 'groundtruthheading 'heading))

(define (set-value code params summary)
  (let ((bus-desc (hash-table/get *bus-code-mapping* code #f)))
    (cond ((not bus-desc) (display "value ") (display code)
	   (display " not found.") (newline))
	  ((eqv? bus-desc 'lateral) 
	   (set-log-snapshot-lateral! summary params)) 
	  ((eqv? bus-desc 'acceleration) 
	   (set-log-snapshot-accel! summary params))
	  ((eqv? bus-desc 'steering) (set-log-snapshot-steer! summary params))
	  ((eqv? bus-desc 'brakes) (set-log-snapshot-brake! summary params))
	  ((eqv? bus-desc 'gear-rotation)
	   (set-log-snapshot-gear-rotate! summary params))
	  ((eqv? bus-desc 'front-wheels)
	   (set-log-snapshot-front-wheel! summary params))
	  ((eqv? bus-desc 'back-wheels)
	   (set-log-snapshot-back-wheel! summary params))
	  ((eqv? bus-desc 'speed)
	   (set-log-snapshot-speed! summary params))
	  ((eqv? bus-desc 'drive-mode)
	   (set-log-snapshot-drive-mode! summary params))
	  ((eqv? bus-desc 'gas-pedal)
	   (set-log-snapshot-gas-pedal! summary params))
	  ((eqv? bus-desc 'ground-truth) ; shuold not be xyz
	   (set-log-snapshot-ground-truth! summary params))
	   (else (set-log-snapshot-heading! summary params))
	)))

;; Fetches and creates a log-interval summary for a particular
;; lower-bound, upper-bound and log
(define (fetch-snapshot time log-filename)
  (let ((snapshot (make-log-snapshot time '0 '0 '0 '0 '0 '0
				    '0 '0 '0 '0 '0 '0)))
    (make-bus-code-mapping)

    (let ((summaries (get-summary-from-log time log-filename)))
      (let loop ((lines summaries))
	(if (null? lines) snapshot
	    (let* ((line (car lines))
		   (code (car line)) 
		   (params (cdr line))
		   (param-length (length params)))
	      (cond ((eqv? param-length 0)
		     (set-value code 'error snapshot))
		    ((eqv? param-length 1)
		     (set-value code (car params) snapshot))
		    (else (set-value code params snapshot)))
	      (loop (cdr lines))))))))

(define epsilon 0.000001)

;; Times are "equal" within 5 significant digits
;; Fixes errors where time input is an integer 
(define (time-equal? input-time given-time)
  (if (<= (abs (- input-time given-time)) epsilon) #t
      #f))

(define (get-summary-from-log time log-filename)
  (with-input-from-file log-filename
    (lambda ()
      (let linelp ((line (read-line)) (summary '())) 
	(if (eof-object? line) summary 
	    (let ((whole-line (with-input-from-string line
				(lambda ()
				  (let lp ((line-list '()))
				    (let ((in (read)))
				      (if (eof-object? in) 
					  (reverse line-list)
					  (lp (cons in
						    line-list)))))))))
		 (let ((line-time (car whole-line)) 
		       (line-params (cdr whole-line)))
		   (cond ((time-equal? line-time time) 
			  (linelp (read-line) (cons line-params summary)))
			  ((< line-time time)
			   (linelp (read-line) summary))
			  (else
			   summary)))))))))
