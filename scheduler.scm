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

;;;; Basic scheduling system

(declare (usual-integrations))

;;; This scheduler maintains a list of jobs that need to be run.  Each
;;; job is a thunk.  Jobs are run serially and are not preempted.
;;; When a job exits (normally) it is forgotten and the next job is
;;; run.  The jobs are permitted to schedule additional jobs,
;;; including rescheduling themselves.  Jobs are presumed idempotent,
;;; and specifically it is assumed acceptable not to count how many
;;; times a given job (by eq?-ness) was scheduled, but merely that it
;;; was scheduled.  When the scheduler runs out of jobs, it returns
;;; the symbol 'done to its caller.

;;; The scheduler supplies an escape mechanism: running the procedure
;;; abort-process, with a value, will terminate the entire job run,
;;; and return the supplied value to the scheduler's caller.
;;; Subsequent calls to the scheduler without first scheduling more
;;; jobs will also return that same value.  If abort-process is called
;;; outside the dynamic extent of a run, it deschedules any jobs that
;;; might be scheduled and saves the value for future refernce as
;;; above.

;;; This scheduler is meant as a low-level support for a propagator
;;; network.  In that use case, the jobs would be propagators that the
;;; network knows need to be run.  Any cells in the network are
;;; invisible to the scheduler, but presumably help the network
;;; schedule more propagators to run (namely those that may be
;;; interested in the cell's goings on).

;;; The public interface is
;;;   (initialize-scheduler)      clears all scheduler state
;;;   (alert-propagators jobs)    schedules a list (or set) of jobs
;;;   (run)                       runs scheduled jobs until done
;;;   (abort-process x)           terminates the run returning x

(define *alerted-propagators*)
(define *alerted-propagator-list*)
(define *abort-process*)
(define *last-value-of-run*)
(define *propagators-ever-alerted*)
(define *propagators-ever-alerted-list*)

(define *universal-ancestor*)
(define *my-parent*)
(define *substitutions*)
(define *premise-outness*)
(define *plunk-counter*)
(define *justification-memory*)

(define (initialize-scheduler)
  (clear-alerted-propagators!)
  (set! *abort-process* #f)
  (set! *last-value-of-run* 'done)
  (set! *propagators-ever-alerted* (make-eq-hash-table))
  (set! *premise-outness* (make-eq-hash-table))
  (set! *propagators-ever-alerted-list*
	(list '*propagators-ever-alerted-list*))
  (set! *universal-ancestor* (list '*universal-ancestor*))
  (set! *my-parent* *universal-ancestor*)
  (set! *substitutions* '())
  (set! *justification-memory* '())
  (set! *number-of-calls-to-fail* 0)
  (set! *plunk-counter* 0)
  'ok)

(define (any-propagators-alerted?)
  (< 0 (hash-table/count *alerted-propagators*)))

(define (clear-alerted-propagators!)
  (set! *alerted-propagators* (make-strong-eq-hash-table))
  (set! *alerted-propagator-list* (list '*alerted-propagator-list*)))

;; Turning this off makes the order in which propagators are run vary
;; chaotically.  That causes the bridge rectifier to fail sporadically
;; (i.e. depending on whether any source files were compiled in the
;; same Scheme session), because its diodes periodically run afoul of
;; *satisfied-requests-stimulate-computation* being off.  The chaotic
;; variation also makes the number of calls to fail vary, occasionally
;; making tests that check it fail.
(define *predictable-run-order* #t)

(define (order-preserving-insert thing table lst)
  (hash-table/lookup
   table
   thing
   (lambda (value) 'ok)
   (lambda ()
     (hash-table/put! table thing #t)
     (push! lst thing))))

(define (push! headed-list thing)
  (set-cdr! headed-list (cons thing (cdr headed-list))))


(define (ordered-key-list table lst)
  (if *predictable-run-order*
      (list-copy (cdr lst))
      (hash-table/key-list table)))

(define (alert-propagators propagators)
  (for-each
   (lambda (propagator)
     (if (not (procedure? propagator))
	 (error "Alerting a non-procedure" propagator))
     (order-preserving-insert
      propagator *propagators-ever-alerted* *propagators-ever-alerted-list*)
     (order-preserving-insert
      propagator *alerted-propagators* *alerted-propagator-list*))
   (listify propagators))
  #f)

(define (alert-all-propagators!)
  (alert-propagators
   (ordered-key-list *propagators-ever-alerted*
		     *propagators-ever-alerted-list*)))

(define (the-alerted-propagators)
  (ordered-key-list *alerted-propagators*
		    *alerted-propagator-list*))

(define (with-process-abortion thunk)
  (call-with-current-continuation
   (lambda (k)
     (fluid-let ((*abort-process* k))
       (thunk)))))

(define termination-trace #f)

(define (abort-process value)
  (if termination-trace
      (ppc `(calling abort-process with ,value and ,*abort-process*)))
  (if *abort-process*
      ;; if the propagator is running
      (begin (clear-alerted-propagators!)         ; TODO Is this right?
	     (*abort-process* value))
      ;; if the user is setting up state
      (begin (clear-alerted-propagators!)         ; TODO Is this right?
	     (set! *last-value-of-run* value))))


;;; For debugging:  GJS
(define *current-propagator*)

(define (run-alerted)
  (let ((temp (the-alerted-propagators)))
    (clear-alerted-propagators!)
    (for-each
     (lambda (propagator)
       (set! *current-propagator* propagator)
       (propagator))
     temp))
  (if (any-propagators-alerted?)
      (run-alerted)
      'done))

(define (run)
  (if (any-propagators-alerted?)
      (set! *last-value-of-run* (with-process-abortion run-alerted)))
  *last-value-of-run*)
