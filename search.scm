;;; ----------------------------------------------------------------------
;;; Copyright 2016 Alexey Radul and Gerald Jay Sussman
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

(declare (usual-integrations))

(define *premise-nogoods* (make-eq-hash-table))

(define (premise-nogoods premise)
  (hash-table/get *premise-nogoods* premise '()))

(define (set-premise-nogoods! premise nogoods)
  (hash-table/put! *premise-nogoods* premise nogoods))

(define (binary-amb cell)
  (let ((true-premise (make-hypothetical cell))
        (false-premise (make-hypothetical cell)))
    (define (amb-choose)
      (let ((reasons-against-true
             (filter all-premises-in?
               (premise-nogoods true-premise)))
            (reasons-against-false
             (filter all-premises-in?
               (premise-nogoods false-premise))))
        (cond ((null? reasons-against-true)
               (kick-out! false-premise)
               (bring-in! true-premise))
              ((null? reasons-against-false)
               (kick-out! true-premise)
               (bring-in! false-premise))
              (else                  ; this amb must fail.
               (kick-out! true-premise)
               (kick-out! false-premise)
               (process-contradictions
                  (pairwise-union reasons-against-true
                                  reasons-against-false)
                  (list me)
                  cell)))))
    ;; The cell is a spiritual neighbor...
    (define me
      (propagator cell '() amb-choose
                  `(binary-amb ,(name cell))))
    (let ((r (list me)))
      ((constant
        (make-tms
         (list (supported #t (list true-premise) r)
               (supported #f (list false-premise) r))))
       cell)
      me)))

(define (process-contradictions nogoods reasons complaining-cell)
  (process-one-contradiction
   (car (sort-by nogoods
          (lambda (nogood)
            (length (filter hypothetical? nogood)))))
   reasons
   complaining-cell))

(define *debugging-contradiction* #f)

(define (process-one-contradiction nogood reasons complaining-cell)
  (let ((hyps (filter hypothetical? nogood)))
    (if (null? hyps)
        (begin 
          (if *debugging-contradiction*
              (bkpt "contradiction" complaining-cell nogood reasons))
          (abort-process
           `(contradiction ,complaining-cell ,nogood ,reasons)))
        (begin
          (kick-out! (car hyps))
          (for-each (lambda (premise)
                      (assimilate-nogood! premise nogood))
                    nogood)))))

(define (assimilate-nogood! premise new-nogood)
  (let ((item (delq premise new-nogood))
        (set (premise-nogoods premise)))
    (if (any (lambda (old) (lset<= eq? old item)) set)
        #f
        (let ((subsumed
               (filter (lambda (old) (lset<= eq? item old))
                       set)))
          (set-premise-nogoods! premise
            (lset-adjoin eq?
              (lset-difference eq? set subsumed) item))))))


(define *number-of-calls-to-fail* 0)

(define (process-nogood! nogood reasons complaining-cell)
  (set! *number-of-calls-to-fail*
        (+ *number-of-calls-to-fail* 1))
  (process-one-contradiction nogood reasons complaining-cell))


(define (require cell)
  ((constant #t) cell))

(define (abhor cell)
  ((constant #f) cell))
