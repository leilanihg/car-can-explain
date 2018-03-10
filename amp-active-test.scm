;;; Crude bias model of common-emitter amplifier.

(initialize-scheduler)

(define TOP (node 'TOP))
(define GND (node 'GND))

(define B (node 'B))
(define C (node 'C))
(define E (node 'E))

(define-cell VThreshold)
(define-cell VSaturation)
(define Q
  ((bjt-crude-bias VThreshold VSaturation 'Q 'active)
   B C E))

(define-cell RB1)
(define RBU ((linear-resistor RB1 'RB1) TOP B))

(define-cell RB2)
(define RBD ((linear-resistor RB2 'RB2) B GND))

(define-cell RC)
(define RPU ((linear-resistor RC 'RC) TOP C))

(define-cell RE)
(define RPD ((linear-resistor RE 'RE) E GND))

(define-cell VCC)
(define VS  ((voltage-source VCC 'VCC) TOP GND))

(cap! TOP)
(cap! GND)
(cap! B)
(cap! C)
(cap! E)

;;; E96 1% values

(tell! VCC 15 'gjs1)
(tell! (potential GND) 0 'gjs2)

(tell! RB2 20000  'gjs3)
(tell! RB1 162000 'gjs4)
(tell! RC  4990   'gjs5)
(tell! RE  1000   'gjs6)

(tell! VThreshold 0.65 'gjs7)		
(tell! VSaturation 0.2 'gjs7)

(cpp (inquire (thing '(current c Q))))
#;
((current c Q) (has-value (*the-nothing*)) (because ()) (depends-on))

#|
;;; We need a voltage-divider slice!

(define-cell R1+R2)
(define n1 (node 'n1))
(define n2 (node 'n2))

(define Rseries
  ((linear-resistor R1+R2 'Rseries) n1 n2))


;;; Install slice.  
;;; Note: do not cap n1 and n2

(sum RB1 RB2 R1+R2)

(identify-terminals (thing '(t1 RB1))
                    (thing '(t1 Rseries)))

(identify-terminals (thing '(t2 RB2))
                    (thing '(t2 Rseries)))


(cpp (inquire (thing '(current c Q))))
#;
((current c Q)
 (has-value 9.983516483523087e-4)
 (because ((- ((i12 Q) (current b Q)) (current c Q))
	   (sum (current b Q) (current c Q) (i12 Q))
	   Q))
 (depends-on (gjs6) (gjs3) (gjs4) (gjs7) (gjs2) (gjs1)))
|#

;;; Without slice, but with plunk, get same result
(plunk! (thing '(potential B)))

(cpp (inquire (thing '(current c Q))))
#;
((current c Q) (has-value 9.983516483516482e-4)
 (because ((- ((i12 Q) (current b Q)) (current c Q))
	   (sum (current b Q) (current c Q) (i12 Q))
	   Q))
 (depends-on (gjs6) (gjs7) (gjs1) (gjs4) (gjs3) (gjs2)))

(cpp (inquire (potential (thing '(e Q)))))
#;
((potential E) (has-value .9983516483516482)
 (because ((- ((potential B) (v13 Q)) (potential E))
	   (sum (v13 Q) (potential E) (potential B))
	   Q))
 (depends-on (gjs7) (gjs1) (gjs4) (gjs3) (gjs2)))

(retract! 'gjs1)

(cpp (inquire (potential (thing '(e Q)))))
#;
((potential e) (has-value (*the-nothing*)))

(assert! 'gjs1)
;Value: done

(cpp (inquire (potential (thing '(e Q)))))
#;
((potential E) (has-value .9983516483516482)
 (because ((- ((potential B) (v13 Q)) (potential E))
	   (sum (v13 Q) (potential E) (potential B))
	   Q))
 (depends-on (gjs7) (gjs1) (gjs4) (gjs3) (gjs2)))

(retract! 'gjs5)

(cpp (inquire (potential (thing '(b Q)))))
#;
((potential B) (has-value 150/91)
 (because (solver)
	  ((+ ((current t2 RB1) (current b Q)) (a B))
	   (sum (current t2 RB1) (current b Q) (a B))
	   B)
	  ((- ((a B) (current t1 RB2)) (a B))
	   (sum (a B) (current t1 RB2) (a B))
	   B))
 (depends-on (gjs1) (gjs4) (gjs3) (gjs2)))

(cpp (inquire (potential (thing '(c Q)))))
#;
((potential c) (has-value (*the-nothing*)))

(tell! RC 15000 'gjs8)
(contradiction #[compound-procedure 29 me]
	       (gjs8 gjs6 gjs7 gjs4 gjs3 gjs2 gjs1)
	       (#[compound-procedure 30 me]
		#[compound-procedure 31 me]))

(retract! 'gjs8)

(tell! RC 10000 'gjs9)

(cpp (inquire (potential (thing '(c Q)))))
#;
((potential C) (has-value 5.016483516483518)
 (because ((- ((potential TOP) (v RC)) (potential C))
	   (sum (v RC) (potential C) (potential TOP))
	   RC))
 (depends-on (gjs9) (gjs6) (gjs7) (gjs4)
	     (gjs3) (gjs2) (gjs1)))

(retract! 'gjs9)
;Value: done

(tell! RC 13900 'gjs10)
;Value: done

(cpp (inquire (potential (thing '(c Q)))))
#;
((potential C) (has-value 1.1229120879120913)
 (because ((- ((potential TOP) (v RC)) (potential C))
	   (sum (v RC) (potential C) (potential TOP))
	   RC))
 (depends-on (gjs10) (gjs6) (gjs7) (gjs4)
	     (gjs3) (gjs2) (gjs1)))

(cpp (inquire (potential (thing '(e Q)))))
#;
((potential E) (has-value .9983516483516482)
 (because ((- ((potential B) (v13 Q)) (potential E))
	   (sum (v13 Q) (potential E) (potential B))
	   Q))
 (depends-on (gjs7) (gjs1) (gjs4) (gjs3) (gjs2)))


