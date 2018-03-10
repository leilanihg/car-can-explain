;;; Synthesis of voltage-divider by slices.
;;;  Numerical values have underbars, e.g. V_S.
;;;  Internal names do not have underbars.

(initialize-scheduler)

(define-cell VI)
(define-cell R1)
(define-cell R2)

(define n1 (node 'n1))
(define n2 (node 'n2))
(define gnd (node 'gnd))

(define VSource ((voltage-source VI 'VS) n1 gnd))

(define RDiv1 ((linear-resistor R1 'R1) n1 n2))
(define RDiv2 ((linear-resistor R2 'R2) n2 gnd))

(define ODiv ((port 'OD) n2 gnd))

(cap! n1)
(cap! n2)
(cap! gnd)

(tell! VI 'V_S 'gjs1)
(tell! (potential gnd) 0 'gjs2)

;;; Declare R1 and R2 to be unknowns worth solving for.
(plunk! R1)
(plunk! R2)

;;; Let's put constraints on the output of the divider.
;;; In particular. let's make it a Thevenin equivalent:

(define n3 (node 'n3))
(define n4 (node 'n4))
(define n5 (node 'n5))

(define-cell VT)
(define-cell RT)

(define VThev ((voltage-source VT 'VT) n3 n5))
(define RThev ((linear-resistor RT 'RT) n3 n4))

(define OThev ((port 'OT) n4 n5))

(cap! n3)
(cap! n4)
(cap! n5)

;;; 20000 = ||(30000, 60000)

(tell! RT 20000 'gjs3)			
(tell! VT (/ 'V_S 3) 'gjs4)
;;; End of Thevenin slice 


;;; Apply Thevenin Slice
(identify-ports ODiv OThev)

;;; Must be true for ANY current.
(tell! (current (thing '(t1 OT))) 'I 'gjs5)

;;; Not enuf to determine these?  Hmmm...

(cpp (inquire R1))
#;
((R1)
 (has-value R1_1)
 (because
  ((/ ((v R1) (current t1 R1)) (R1))
   (product (R1) (current t1 R1) (v R1))
   R1)
  ((plunker)))
 (depends-on (gjs1) (gjs2) (gjs3) (gjs5) (gjs4) (premise-R1_1)))

(cpp (inquire R2))
#;
((R2)
 (has-value (/ (+ (* 60000 I R1_1) (* -1 R1_1 V_S))
               (+ (* 3 I R1_1) (* -60000 I) (* -2 V_S))))
 (because
  ((plunker))
  ((/ ((v R2) (current t1 R2)) (R2))
   (product (R2) (current t1 R2) (v R2))
   R2)
  ((solver))
  (R1)
  ((+ ((current t1 R1) (current t2 R1)) (zero-i R1))
   (sum (current t1 R1) (current t2 R1) (zero-i R1))
   R1))
 (depends-on (premise-R2_2) (premise-R1_1) (gjs4) (gjs5) (gjs3) (gjs2) (gjs1))))

(premise-in? 'premise-R1_13)
;Value: #t

(premise-in? 'premise-R2_14)
;Value: #t

;;; Note: R2 is assigned to a value that depends on the plunk variable and its premise. 
;;;       That value does not depend on the solver...
;;; The equations never got to the solver.
;;; This is why both premises are IN.

(pp *substitutions*)
(((= R2_2
     (/ (+ (* 60000 I R1_1) (* -1 R1_1 V_S))
        (+ (* 3 I R1_1) (* -60000 I) (* -2 V_S))))
  (((premise-R2_2 premise-R1_1 gjs4 gjs5 gjs3 gjs2 gjs1)
    ((solver)
     #[compound-procedure 101 ...]
     #[compound-procedure 100 ...])))))

;;; Indeed, there seems to be nothing more to do:

