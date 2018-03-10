;;; ----------------------------------------------------------------------
;;; Copyright 2016 Alexey Radul, Gerald Jay Sussman, Leilani H Gilpin
;;; and Ben Z Yuan 
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

;;; Wheel speed is the direction combined with the 
(define-structure
  (log-interval-qual
   (type vector) (named 'log-interval-qual) (print-procedure #f))
  interval
  lateral accel steer brake gear-rotate
  left-front-wheel right-front-wheel 
  left-back-wheel right-back-wheel
  speed drive-mode-1 drive-mode-2 drive-mode-3 
  gas-pedal ground-truth-x ground-truth-y ground-truth-z heading)

(define summary-length 17)

(define log-interval-qual-equal? equal?)

(define (->log-interval-qual x)
  (if (log-interval-qual? x)
      x
      (make-log-interval-qual (make-interval 0 0)
				      '0 '0 '0 '0 '0 '0 '0 '0 
				      '0 '0 '0 '0 '0 '0 '0 '0))) 

(define (make-qual-direction-from-change angle wheel-rotation-qual)
  (let ((speed-desc (qualitative-description wheel-rotation-qual)))
    (cond ((> 0. angle) (make-qualitative-direction 'left speed-desc))
	  ((< 0. angle) (make-qualitative-direction 'right speed-desc))
	  ((or (eqv? 0. angle) (eqv? 0 angle))
	   (make-qualitative-direction '0 speed-desc))
	  (else (make-qualitative-direction '? speed-desc)))))

(define (make-qual-direction-from-accel angle wheel-rotation-qual)
  (let ((speed-desc (qualitative-description wheel-rotation-qual)))
    (cond ((> 0. angle) (make-qualitative-direction 'right speed-desc))
	  ((< 0. angle) (make-qualitative-direction 'left speed-desc))
	  ((or (eqv? 0. angle) (eqv? 0 angle))
	   (make-qualitative-direction '0 speed-desc))
	  (else (make-qualitative-direction '? speed-desc)))))

(define (make-qual-from-change delta)
  (cond ((> 0. delta) (make-qualitative 'inc)); positive
	((< 0. delta) (make-qualitative 'dec))
	((or (eqv? 0. delta) (eqv? 0 delta)) 
	     (make-qualitative '0))
	 (else (make-qualitative '?))))

(define (set-ground-truth low-snapshot high-snapshot qual-summary)
  (if (and (list? (log-snapshot-ground-truth high-snapshot))
	   (list? (log-snapshot-ground-truth low-snapshot)))
      (let ((ground-truth-x-delta 
	     (- (car (log-snapshot-ground-truth high-snapshot))
		(car (log-snapshot-ground-truth low-snapshot))))
	    (ground-truth-y-delta 
	     (- (cadr (log-snapshot-ground-truth high-snapshot))
		(cadr (log-snapshot-ground-truth low-snapshot))))
	    (ground-truth-z-delta 
	     (- (caddr (log-snapshot-ground-truth high-snapshot))
		(caddr (log-snapshot-ground-truth low-snapshot)))))
	(set-log-interval-qual-ground-truth-x! 
	 qual-summary (make-qual-from-change ground-truth-x-delta))
	(set-log-interval-qual-ground-truth-y! 
	 qual-summary (make-qual-from-change ground-truth-y-delta))
	(set-log-interval-qual-ground-truth-z! 
	 qual-summary (make-qual-from-change ground-truth-z-delta)))

      (begin
	(set-log-interval-qual-ground-truth-x! qual-summary 
					   (make-qualitative '?))
	(set-log-interval-qual-ground-truth-y! qual-summary 
					   (make-qualitative '?))
	(set-log-interval-qual-ground-truth-z! qual-summary 
					   (make-qualitative '?)))))

(define (set-drive-mode low-snapshot high-snapshot qual-summary)
  (if (and (list? (log-snapshot-drive-mode high-snapshot))
	   (list? (log-snapshot-drive-mode low-snapshot)))
      (let ((drive-mode-1-delta 
	     (- (car (log-snapshot-drive-mode high-snapshot))
		(car (log-snapshot-drive-mode low-snapshot))))
	    (drive-mode-2-delta 
	     (- (cadr (log-snapshot-drive-mode high-snapshot))
		(cadr (log-snapshot-drive-mode low-snapshot))))
	    (drive-mode-3-delta 
	     (- (caddr (log-snapshot-drive-mode high-snapshot))
		(caddr (log-snapshot-drive-mode low-snapshot)))))
	(set-log-interval-qual-drive-mode-1! 
	 qual-summary (make-qual-from-change drive-mode-1-delta))
	(set-log-interval-qual-drive-mode-2! 
	 qual-summary (make-qual-from-change drive-mode-2-delta))
	(set-log-interval-qual-drive-mode-3! 
	 qual-summary (make-qual-from-change drive-mode-3-delta)))
      (begin
	(set-log-interval-qual-drive-mode-x! qual-summary 
					     (make-qualitative '?))
	(set-log-interval-qual-drive-mode-y! qual-summary 
					     (make-qualitative '?))
	(set-log-interval-qual-drive-mode-z! qual-summary 
					     (make-qualitative '?)))))

(define (set-qual-summary low-snapshot high-snapshot qual-summary)
  (pp low-snapshot)
  (pp high-snapshot)
  (set-log-interval-qual-interval! qual-summary 
				   (make-interval
				    (log-snapshot-time low-snapshot)
				    (log-snapshot-time high-snapshot)))
  (if (and (log-snapshot? low-snapshot) (log-snapshot? high-snapshot))
      (let ((lateral-delta (-  (log-snapshot-lateral high-snapshot)
				(log-snapshot-lateral low-snapshot)))
	    (accel-delta (- (log-snapshot-accel high-snapshot)
			    (log-snapshot-accel low-snapshot)))
	    (steer-delta (- (log-snapshot-steer high-snapshot)
			    (log-snapshot-steer low-snapshot)))
	    (brake-delta (- (log-snapshot-brake high-snapshot)
			    (log-snapshot-brake low-snapshot)))
	    (gear-rotate-delta (- (log-snapshot-gear-rotate high-snapshot)
				  (log-snapshot-gear-rotate low-snapshot)))
	    (left-front-delta (- (car (log-snapshot-front-wheel high-snapshot))
				 (car (log-snapshot-front-wheel low-snapshot))))
	    (right-front-delta 
	     (- (cadr (log-snapshot-front-wheel high-snapshot))
		(cadr (log-snapshot-front-wheel low-snapshot))))
	    (left-back-delta 
	     (- (car (log-snapshot-back-wheel high-snapshot))
		(car (log-snapshot-back-wheel low-snapshot))))
	    (right-back-delta 
	     (- (cadr (log-snapshot-back-wheel high-snapshot))
		(cadr (log-snapshot-back-wheel low-snapshot))))
	    (speed-delta (- (log-snapshot-speed high-snapshot)
		            (log-snapshot-speed low-snapshot)))
	    (gas-pedal-delta (- (log-snapshot-gas-pedal high-snapshot)
				(log-snapshot-gas-pedal
				 low-snapshot)))
	    (heading-delta (- (log-snapshot-heading high-snapshot)
		              (log-snapshot-heading low-snapshot))))
      ; set the values and make qualitatives
	(set-log-interval-qual-lateral! 
	 qual-summary (make-qual-direction-from-accel ; actual value 
		       (log-snapshot-lateral high-snapshot)
		       (make-qual-from-change accel-delta)))
	 ;qual-summary (make-qual-from-change lateral-delta))
	(set-log-interval-qual-accel! 
	 qual-summary (make-qual-from-change accel-delta)) 
;	(set-log-interval-qual-steer! 
;	 qual-summary (make-qual-from-change steer-delta)) 
	(set-log-interval-qual-steer! 
	 qual-summary (make-qual-direction-from-change steer-delta 
						       (make-qual-from-change
							left-front-delta))) 
	(set-log-interval-qual-brake! 
	 qual-summary (make-qual-from-change brake-delta)) 
	(set-log-interval-qual-gear-rotate! 
	 qual-summary (make-qual-from-change gear-rotate-delta))
	(set-log-interval-qual-left-front-wheel! 
	 qual-summary (make-qual-from-change left-front-delta)) 
	(set-log-interval-qual-right-front-wheel!
	 qual-summary (make-qual-from-change right-front-delta)) 
	(set-log-interval-qual-left-back-wheel! 
	 qual-summary (make-qual-from-change left-back-delta))
	(set-log-interval-qual-right-back-wheel! 
	 qual-summary (make-qual-from-change right-back-delta))
	(set-log-interval-qual-speed! 
	 qual-summary (make-qual-from-change speed-delta)) 
	(set-log-interval-qual-gas-pedal! 
	 qual-summary (make-qual-from-change gas-pedal-delta))
	(set-log-interval-qual-heading! 
	 qual-summary (make-qual-from-change heading-delta))
	(set-ground-truth low-snapshot high-snapshot qual-summary)
	(set-drive-mode low-snapshot high-snapshot qual-summary)
      qual-summary)
  (display "need two snapshots to be able to set values")))
