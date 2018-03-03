(define (n-channel-mosfet K VT
			#!optional given-name state)
  (3-terminal-device '(nmos g d s) given-name
    (lambda (vGS iG vDS iD)
      (let-cells (vov vovsq K/2 iDsat t1 t2 vDS/2 iDohmic
                  not-cutoff not-ohmic not-not-ohmic
                  cutoff saturated ohmic
                  (zero 0) (two 2))
        (same iG zero)
        (sum vov VT vGS)
        (<? VT vGS not-cutoff)
        (<? vov vDS not-ohmic)
        (inverter not-cutoff cutoff)
        (controlled-same zero iD cutoff)
        (inverter not-ohmic not-not-ohmic)
        (conjunction not-cutoff not-not-ohmic ohmic)
        (product vDS/2 two vDS)
        (sum t2 vDS/2 vov)
        (product t2 vDS t1)
        (product K t1 iDohmic)
        (controlled-same iDohmic iD ohmic)
        (conjunction not-cutoff not-ohmic saturated)
        (quadratic vov vovsq)
        (product K/2 two K)
        (product vovsq K/2 iDsat)
        (controlled-same iDsat iD saturated)
        (choose-exactly-one ohmic cutoff saturated)
	(if (default-object? state)
	    (begin (binary-amb cutoff)
                   (binary-amb saturated))
	    (case state
	      ((ohmic) ((constant #t) ohmic)
                       ((constant #f) cutoff)
                       ((constant #f) saturated))
	      ((saturated) ((constant #f) ohmic)
			   ((constant #f) cutoff)
			   ((constant #t) saturated))
	      ((cutoff) ((constant #f) ohmic)
			((constant #t) cutoff)
			((constant #f) saturated))
	      (else (error "Bad BJT state" state)))))
      trivial-insides)
    K VT))

(define (p-channel-mosfet K VT
			#!optional given-name state)
  (3-terminal-device '(pmos g d s) given-name
    (lambda (vSG iG vSD iD)
      (let-cells (vov vovsq K/2 iDsat t1 t2 vDS/2 iDohmic
                  not-cutoff not-ohmic not-not-ohmic
                  cutoff saturated ohmic
                  (zero 0) (two 2))
        (same iG zero)
        (sum vov VT vSG)
        (<? VT vov not-cutoff)
        (<? VT vSD not-ohmic)
        (inverter not-cutoff cutoff)
        (controlled-same zero iD cutoff)
        (inverter not-ohmic not-not-ohmic)
        (conjunction not-cutoff not-not-ohmic ohmic)
        (product vSD/2 two vSD)
        (sum t2 vSD/2 vov)
        (product t2 vSD t1)
        (product K t1 iDohmic)
        (controlled-same iDohmic iD ohmic)
        (conjunction not-cutoff not-ohmic saturated)
        (quadratic vov vovsq)
        (product K/2 two K)
        (product vovsq K/2 iDsat)
        (controlled-same iDsat iD saturated)
        (choose-exactly-one ohmic cutoff saturated)
	(if (default-object? state)
	    (begin (binary-amb cutoff)
                   (binary-amb saturated))
	    (case state
	      ((ohmic) ((constant #t) ohmic)
                       ((constant #f) cutoff)
                       ((constant #f) saturated))
	      ((saturated) ((constant #f) ohmic)
			   ((constant #f) cutoff)
			   ((constant #t) saturated))
	      ((cutoff) ((constant #f) ohmic)
			((constant #t) cutoff)
			((constant #f) saturated))
	      (else (error "Bad BJT state" state)))))
      trivial-insides)
    K VT))

#|
(initialize-scheduler)

(define TOP (node 'TOP))
(define GND (node 'GND))

(define-cell K 0.02)
(define-cell VT 1.0)

;;(define S (node 'S))
(define G (node 'G))
(define D (node 'D))

(define Q
  ((n-channel-mosfet K VT 'Q 'saturated) G D GND))

(define-cell RD)
(define RPU
  ((linear-resistor RD 'RD) TOP D))


(define-cell VCC)
(define VPS  ((voltage-source VCC 'VCC) TOP GND))

(define-cell VIN)
(define VD  ((voltage-source VIN 'VIN) G GND))

(cap! TOP)
(cap! GND)
(cap! G)
(cap! D)
;;(cap! S)

(tell! VCC 15 'gjs1)
(tell! (potential GND) 0 'gjs2)

(tell! RD 1000  'gjs3)

(tell! VIN 1.5  'gjs4)

(cpp (inquire (thing '(current d Q))))
#;
((current d Q) (has-value .0025) (because ()) (depends-on (gjs2) (gjs4)))

(cpp (inquire (thing '(potential D))))
#;
((potential D)
 (has-value 12.5)
 (because ((- ((potential TOP) (v RD)) (potential D))
	   (sum (v RD) (potential D) (potential TOP))
	   RD))
 (depends-on (gjs3) (gjs4) (gjs2) (gjs1)))

|#

#|
(initialize-scheduler)

(define TOP (node 'TOP))
(define GND (node 'GND))

(define-cell K 2/1000)
(define-cell VT 1)

(define S (node 'S))
(define G (node 'G))
(define D (node 'D))

(define Q
  ((n-channel-mosfet K VT 'Q 'saturated) G D S))

(define-cell RRd)
(define RD
  ((linear-resistor RRd 'RD) TOP D))


(define-cell RRs)
(define RS
  ((linear-resistor RRs 'RS) S GND))

(define-cell VCC)
(define VPS  ((voltage-source VCC 'VCC) TOP GND))

(define-cell VIN)
(define VD  ((voltage-source VIN 'VIN) G GND))

(cap! TOP)
(cap! GND)
(cap! G)
(cap! D)
(cap! S)

(tell! VCC 15 'gjs1)
(tell! (potential GND) 0 'gjs2)
(tell! RRd 5000  'gjs3)
(tell! RRs 1000  'gjs4)

(tell! VIN 3  'gjs5)

(cpp (inquire (thing '(current d Q))))

;(tell! (thing '(vov Q)) 1 'bar)
;(plunk! (thing '(vov Q)))
;(tell! (thing '(current d Q)) 1/1000 'foo)
;(plunk! (thing '(current d Q)))
;;; Plunks are failing because wrong root of quadratic is found.

(cpp (inquire (thing '(current d Q))))

(cpp (inquire (thing '(potential D))))

|#