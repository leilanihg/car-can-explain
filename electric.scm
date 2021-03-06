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

(declare (usual-integrations make-cell cell?))

(define (physob things to-build #!optional given-name)
  (let ((things (listify things)))
    (define (me message)
      (case message
	((name) given-name)
	((set-name!)
	 (lambda (new)
	   (set! given-name new)
	   (eq-put! me 'name given-name)))
	((things) things)
	(else
	 (or (fluid-let ((*my-parent* me))
               (insides message))
	     (error "Unknown message" message)))))
    (if (default-object? given-name)
	((me 'set-name!) me)
	((me 'set-name!) given-name))
    (define insides
      (fluid-let ((*my-parent* me))
	(to-build)))
    (eq-put! me 'physob #t)
    (eq-put! me 'action to-build)
    (eq-put! me 'parent *my-parent*)
    (eq-adjoin! *my-parent* 'children me)
    me))

(define (physob? x)
  (eq-get x 'physob))


;;; Primitive data structure

(define (electrical-terminal node-potential)
  (physob (list node-potential)
    (lambda ()
      (let-cells (current)
	(define (insides message)
	  (case message
	    ((current) current)
	    ((potential) node-potential)
	    (else #f)))
	insides))))	  

(define (current terminal)
  (terminal 'current))

(define (potential terminal)
  (terminal 'potential))

(define (identify-terminals t1 t2)
  (same (potential t1) (potential t2))
  (same (current t1) (current t2)))

;;; We measure the voltage from t1 to t2 (i.e. v = e(t1) - e(t2)), and
;;; the current i1 is measured as flowing into t1.  This sign
;;; convention makes the product p=v*i1 be the power entering the
;;; device.

(define *my-insides*)	    ;used here and in 3-terminal device

(define ((2-terminal-device type given-name vic . params) n1 n2)
  (physob (list n1 n2)
    (lambda ()
      (let ((terminal-names
             (if (list? type) (cdr type) '(t1 t2))))
        (let ((t1 (n1 (car terminal-names)))
              (t2 (n2 (cadr terminal-names))))
          (let ((i1 (current t1)) (e1 (potential t1))
                (i2 (current t2)) (e2 (potential t2)))
            (let-cells (v P (zero-i 0))
              (sum v e2 e1)
              (sum i1 i2 zero-i)
              (product i1 v P)
              (define (insides message)
                (case message
                  ((terminals) (list t1 t2))
                  ((nodes) (list n1 n2))
                  ((currents) (list i1 i2))
                  ((potentials) (list e1 e2))
                  ((voltage) v)
                  ((voltages) (list v))
                  ((power) P)
                  ((powers) (list P))
                  ((parameters) params)
                  ((type) type)
                  (else #f)))
              (fluid-let ((*my-insides* insides))
		(vic v i1))
              insides)))))
    (if (default-object? given-name)
        (let ((type (if (list? type) (car type) type)))
          `(,type ,(map name params)
                  ,(name n1) ,(name n2)))
        given-name)))

;;; In a three-terminal device we measure the voltage v13 from t1 to
;;; t3 and the voltage v23 from t2 to t3.  The current i1 is measured
;;; as flowing into t1 and the current i2 is measured as flowing into
;;; t2.  This sign convention makes the product p=v*i1 be the power
;;; entering the device.

(define ((3-terminal-device type given-name vic . params) n1 n2 n3)
  (physob (list n1 n2 n3)
    (lambda ()
      (let ((terminal-names
             (if (list? type) (cdr type) '(t1 t2 t3))))
        (let ((t1 (n1 (car terminal-names)))
              (t2 (n2 (cadr terminal-names)))
              (t3 (n3 (caddr terminal-names))))
          (let ((i1 (current t1)) (e1 (potential t1))
                (i2 (current t2)) (e2 (potential t2))
                (i3 (current t3)) (e3 (potential t3)))
	    (let-cells (v13 v23 P1 P2 P i12 (zero-i 0))
              (sum v13 e3 e1)
              (sum v23 e3 e2)
              (sum i1 i2 i12)
              (sum i12 i3 zero-i)
              (product i1 v13 P1)
              (product i2 v23 P2)
              (sum P1 P2 P)
              (define (insides message)
                (case message
                  ((terminals) (list t1 t2 t3))
                  ((nodes) (list n1 n2 n3))
                  ((currents) (list i1 i2 i3))
                  ((potentials) (list e1 e2 e3))
                  ((voltages) (list v13 v23))
                  ((powers) (list P1 P2 P))
                  ((power) P)
                  ((parameters) params)
                  ((type) type)
                  (else #f)))
	      (fluid-let ((*my-insides* insides))
		(vic v13 i1 v23 i2))
              insides)))))
    (if (default-object? given-name)
        (let ((type (if (list? type) (car type) type)))
          `(,type ,(map name params)
                  ,(name n1) ,(name n2) ,(name n3)))
        given-name)))

(define (terminals device)
  (device 'terminals))

(define (nodes device)
  (device 'nodes))

(define (currents device)
  (device 'currents))

(define (potentials device)
  (device 'potentials))

(define (voltage device)
  (device 'voltage))

(define (voltages device)
  (device 'voltages))

(define (power device)
  (device 'power))

(define (powers device)
  (device 'powers))

(define (parameters device)
  (device 'parameters))

(define (trivial-insides message) #f)


(define (my-terminal terminal-name)
  (let ((my-terminals (terminals *my-insides*)))
    (find (lambda (terminal)
	    (eqv? (car (name terminal)) terminal-name))
	  my-terminals)))

(define (my-node terminal-name)
  (let ((my-terminals (terminals *my-insides*))
	(my-nodes (nodes *my-insides*)))
    (list-ref my-nodes
      (list-index (lambda (terminal)
		    (eqv? (car (name terminal))
			  terminal-name))
		  my-terminals))))

;;; A Node makes up terminals to be served to devices

(define (node #!optional given-name)
  (define me
    (node-helper
     (if (default-object? given-name)
         me
         given-name)))
  (eq-put! me 'node #t)
  (eq-put! me 'parent *my-parent*)
  (eq-adjoin! *my-parent* 'children me)  
  me)

(define (node? thing)
  (eq-get thing 'node))

(define (capped? node)
  (assert (node? node))
  (node 'capped?))

(define (cap! node)
  (assert (node? node))
  (node 'cap!))

(define (node-helper given-name)
  (let-cells (potential)
    (let ((currents '()) (capped? #f))
      (define (make-terminal terminal-name)
        (assert (not capped?) "Node capped" (name me))
        (let ((t (electrical-terminal potential)))
	  (name! t terminal-name)
	  (set! currents (cons (t 'current) currents))
	  t))
      (define (cap!)
        (assert (not capped?) "Node already capped" (name me))
	(assert (not (= (length currents) 0))
		"Node has no currents"
		(name me))
        (set! capped? #t)
        (fluid-let ((*my-parent* me))
          (let ((isum 
                 (let lp ((n (length currents)) (is currents))
                   (cond ((= n 1) is)
                         ((even? n)
                          (let ((n/2 (/ n 2)))
                            (let ((a1 (lp n/2 (list-head is n/2)))
                                  (a2 (lp n/2 (list-tail is n/2))))
			      (let-cells (a)
				(sum (car a1) (car a2) a)
				(list a)))))
                         ((odd? n)
                          (let ((a1 (lp (- n 1) (cdr is)))
                                (i2 (car is)))
			    (let-cells (a)
			      (sum (car a1) i2 a)
			      (list a))))
                         (else (error))))))
            ((constant 0) (car isum)))))
      (define (me #!optional message)
        (case message
          ((name) given-name)
          ((set-name!)
           (lambda (new)
             (set! given-name new)
             (eq-put! me 'name given-name)))
          ((potential) potential)
          ((currents) currents)
          ((capped?) capped?)
          ((cap!) (cap!))
          (else (make-terminal message))))
      (eq-put! me 'physob #t)
      (eq-delete! (parent potential) 'children me)
      (eq-put! potential 'parent me)
      (eq-adjoin! me 'children potential)
      me)))

(define (linear-resistor resistance #!optional given-name)
  (2-terminal-device 'resistor given-name
   (lambda (v i)
     (product resistance i v)
     trivial-insides)
   resistance))

(define (voltage-source strength #!optional given-name)
  (2-terminal-device 'voltage-source given-name
   (lambda (v i)
     (same strength v)
     trivial-insides)
   strength))

(define (current-source strength #!optional given-name)
  (2-terminal-device 'current-source given-name
   (lambda (v i)
     (same strength i)
     trivial-insides)
   strength))

(define (ideal-diode v-threshold #!optional given-name)
  (2-terminal-device '(diode anode cathode) given-name
   (lambda (v i)
     (let-cells ((if>=0 #t) (vr<=VT #t) vreverse
                 iforward conducting -conducting
                 (zero-v 0) (zero-i 0))
       ;;#t=>conducting; #f=>not conducting
       (binary-amb conducting)
       (inverter conducting -conducting)
       (spst-switch conducting v-threshold v)
       (spst-switch conducting i iforward)
       (>=? iforward zero-i if>=0)

       (spst-switch -conducting v vreverse)
       (<=? vreverse v-threshold vr<=VT)
       (spst-switch -conducting zero-i i)
       trivial-insides))
   v-threshold))

(define (bjt-crude-bias VBEthreshold VCEsaturation
			#!optional given-name state)
  (3-terminal-device '(bjt-crude-bias b c e) given-name
    (lambda (VBE IB VCE IC)
      (let-cells (type                ;+ for NPN, - for PNP
                  VBEsign VCEsign IBsign ICsign
                  absVBEthreshold absVCEsaturation
		  absVBE absVCE absIC
                  cutoff saturated inactive active
                  (zeroI 0))
        (sign VBEthreshold type)
        (same VBEsign type)
        (sign VBE VBEsign)
        (same VCEsign type)
        (sign VCE VCEsign)
        (same IBsign type)
        (sign IB IBsign)
        (same ICsign type)
        (sign IC ICsign)
        (absolute-value VBEthreshold absVBEthreshold)
        (absolute-value VBE absVBE)
        (absolute-value VCEsaturation absVCEsaturation)
        (controlled-same VBE VBEthreshold saturated)
        (controlled-same VCE VCEsaturation saturated)
        (>? absVCE absVCEsaturation active) ;not saturated
        (controlled-same VBE VBEthreshold active)
        (controlled-same IB zeroI active)
        (<? absVBE absVBEthreshold cutoff)
        (controlled-same IB zeroI cutoff)
        (controlled-same IC zeroI cutoff)

        ;; At most one can be true
        (choose-exactly-one active cutoff saturated)
	
	(if (default-object? state)
	    (begin (binary-amb active)
                   (binary-amb saturated))
	    (case state
	      ((active) ((constant #t) active)
			((constant #f) cutoff)
			((constant #f) saturated))
	      ((saturated) ((constant #f) active)
			   ((constant #f) cutoff)
			   ((constant #t) saturated))
	      ((cutoff) ((constant #f) active)
			((constant #t) cutoff)
			((constant #f) saturated))
	      (else (error "Bad BJT state" state)))))
      trivial-insides)
    VBEthreshold VCEsaturation))

;;; Useful for making abstractions.  One terminal is connected to an
;;; inside node, the other is exported as a receiver for an external
;;; node.

(define (connector #!optional given-name)
  (2-terminal-device 'connector given-name
   (lambda (v i)
     ((constant 0) v)
     trivial-insides)))

(define (ground node)
  ((constant 0)
   (potential node)))

#|
;;; Trivial test

(initialize-scheduler)

(define-cell V)
(define-cell R)

(define n1 (node 'n1))
(define gnd (node 'gnd))

(define VS
  ((voltage-source V 'VS) n1 gnd))

(define RL
  ((linear-resistor R 'RL) n1 gnd))

(cap! n1)
(cap! gnd)

(tell! V 15 'gjs1)
(tell! R 1000 'gjs2)
(tell! (potential gnd) 0 'gjs3)


(cpp (content (cadr (gnd 'currents))))
#;
#(tms (#(supported 3/200
                   (gjs2 gjs3 gjs1)
                   (#[compound-procedure 34 ...]))))
;;; good!

(cpp (name (unhash 34)))
#;
((- ((a gnd) (current t2 RL)) (current t2 VS))
 (sum (current t2 RL) (current t2 VS) (a gnd))
 gnd)

(cpp (content (car (gnd 'currents))))
#;
#(tms (#(supported -3/200
                   (gjs2 gjs3 gjs1)
                   (#[compound-procedure 35 me]))))
;;; good!

;;; Naming is correct!
(name (cadr (currents n1)))
;Value: (current t1 VS)

;;; Thing is OK
(thing '(current t1 RL))
#| me |#

(name (thing '(current t1 RL)))
#|
(current t1 RL)
|#
|#

#|
;;; Voltage-divider

(initialize-scheduler)

(define-cell V)
(define-cell R1)
(define-cell R2)

(define n1 (node 'n1))
(define n2 (node 'n2))
(define gnd (node 'gnd))

(define VS
  ((voltage-source V 'VS) n1 gnd))

(define RL1
  ((linear-resistor R1 'RL1) n1 n2))
(define RL2
  ((linear-resistor R2 'RL2) n2 gnd))

(cap! n1)
(cap! n2)
(cap! gnd)


(tell! V 15 'gjs1)

(tell! R1 1000 'gjs2)

(tell! R2 2000 'gjs3)

(tell! (potential gnd) 0 'gjs4)


(cpp (inquire (potential n2)))
#;
((potential n2)
 (has-value (*the-nothing*))
 (because ())
 (depends-on))
;;; Of course! no propagation.

;;; We need a slice!

(define-cell R1+R2)
(define n3 (node 'n3))
(define n4 (node 'n4))

(define Rseries
  ((linear-resistor R1+R2 'Rseries) n3 n4))


;;; Install slice.  
;;; Note: do not cap n3 and n4

(sum R1 R2 R1+R2)

(identify-terminals (thing '(t1 RL1))
                    (thing '(t1 Rseries)))

(identify-terminals (thing '(t2 RL2))
                    (thing '(t2 Rseries)))


(cpp (inquire (potential n2)))
#;
((potential n2) (has-value 10)
 (because ((- ((potential n1) (v RL1)) (potential n2))
	   (sum (v RL1) (potential n2) (potential n1))
	   RL1))
 (depends-on (gjs2) (gjs3) (gjs4) (gjs1)))

;;; Slice worked!
|#

#|
;;; Testing 3-terminal nodes

(initialize-scheduler)

(define-cell V)
(define-cell I)
(define-cell R1)
(define-cell R2)

(define n1 (node 'n1))
(define n2 (node 'n2))
(define gnd (node 'gnd))

(define VS
  ((voltage-source V 'VS) n2 gnd))

(define RL1
  ((linear-resistor R1 'RL1) n1 n2))
(define RL2
  ((linear-resistor R2 'RL2) n2 gnd))
(define IS
  ((current-source I 'IS) n1 gnd))

(cap! n1)
(cap! n2)
(cap! gnd)

(tell! (potential gnd) 0 'gjs1)

(tell! V 10 'gjs2)

(tell! I 0.01 'gjs3)

(tell! R1 1000 'gjs4)

(tell! R2 2000 'gjs5)

(cpp (inquire (potential n2)))
#;
((potential n2) (has-value 10)
 (because ((+ ((v VS) (potential gnd)) (potential n2))
	   (sum (v VS) (potential gnd) (potential n2))
	   VS))
 (depends-on (gjs1) (gjs2)))

(cpp (inquire (potential n1)))
#;
((potential n1) (has-value 0.)
 (because ((+ ((v RL1) (potential n2)) (potential n1))
	   (sum (v RL1) (potential n2) (potential n1))
	   RL1))
 (depends-on (gjs2) (gjs1) (gjs3) (gjs4)))

(cpp (inquire (thing '(current t1 VS))))
#;
((current t1 VS) (has-value -.015)
 (because ((- ((a n2) (current t2 RL1)) (current t1 VS))
	   (sum (current t2 RL1) (current t1 VS) (a n2))
	   n2))
 (depends-on (gjs3) (gjs5) (gjs1) (gjs2)))
|#
