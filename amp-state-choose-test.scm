;;; Now, let the transistor decide the state

(initialize-scheduler)

(define TOP (node 'TOP))

(define GND (node 'GND))

(define B (node 'B))

(define C (node 'C))

(define E (node 'E))

(define-cell VThreshold)
(define-cell VSaturation)
(define Q
  ((bjt-crude-bias VThreshold VSaturation 'Q)
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

(cpp (inquire (thing '(potential B))))
#;
((potential B) (has-value (*the-nothing*)) (because ()) (depends-on))

(plunk! (thing '(potential B)))

(cpp (inquire (thing '(potential B))))
#;
((potential B) (has-value 150/91)
 (because (solver)
	  ((+ ((current t2 RB1) (current b Q)) (a B))
	   (sum (current t2 RB1) (current b Q) (a B))
	   B)
	  ((- ((a B) (current t1 RB2)) (a B))
	   (sum (a B) (current t1 RB2) (a B))
	   B))
 (depends-on (gjs1) (gjs4) (hypothetical) (gjs3) (gjs2)))

(cpp (inquire (thing '(current c Q))))
#;
((current c Q) (has-value 9.983516483516482e-4)
 (because ((- ((i12 Q) (current b Q)) (current c Q))
	   (sum (current b Q) (current c Q) (i12 Q))
	   Q))
 (depends-on (gjs6) (gjs7) (gjs1) (gjs4)
	     (hypothetical) (gjs3) (gjs2)))

(cpp (inquire (thing '(active Q))))
#;
((active Q) (has-value #t)
 (because ((binary-amb (active Q)) Q))
 (depends-on (hypothetical)))

(cpp (inquire (thing '(potential C))))
#;
((potential C) (has-value 10.018225274725275)
 (because ((- ((potential TOP) (v RC)) (potential C))
	   (sum (v RC) (potential C) (potential TOP))
	   RC))
 (depends-on (gjs5) (gjs6) (gjs7) (gjs4)
             (hypothetical) (gjs3) (gjs2) (gjs1)))

(retract! 'gjs5)

(cpp (inquire (thing '(potential C))))
#;
((potential C) (has-value (*the-nothing*)))

(cpp (inquire (thing '(current c Q))))
;;; unchanged, because did not depend on gjs5

(tell! RC 15000 'gjs8)

(cpp (inquire (thing '(active Q))))
#;
((active Q) (has-value #f)
 (because ((binary-amb (active Q)) Q))
 (depends-on (hypothetical)))

(cpp (inquire (thing '(saturated Q))))
#;
((saturated Q) (has-value #f)
 (because ((binary-amb (saturated Q)) Q))
 (depends-on (hypothetical)))

;;;*** Wrong! should be saturated.

(cpp (inquire (thing '(cutoff Q))))
#;
((cutoff Q) (has-value #t)
 (because ((function-propagator
	    (#[compiled-closure 37 ...])
	    ((u+v
	      (choose-exactly-one (active Q)
				  (cutoff Q)
				  (saturated Q))
	      Q)
	     (active Q))
	    (cutoff Q))
	   (disjunction
	    (active Q)
	    (cutoff Q)
	    (u+v (choose-exactly-one (active Q)
				     (cutoff Q)
				     (saturated Q))
		 Q))
	   (choose-exactly-one (active Q)
			       (cutoff Q)
			       (saturated Q))
	   Q))
 (depends-on (hypothetical) (hypothetical)))

(cpp (inquire (thing '(current c Q))))
#;
((current c Q)
 (has-value 0)
 (because ())
 (depends-on (hypothetical) (hypothetical)))

(cpp (inquire (thing '(potential B))))
#;
((potential B) (has-value (*the-nothing*)))

;;; Nope!...

(cpp (inquire (thing '(current b Q))))
#;
((current b Q)
 (has-value 0)
 (because ())
 (depends-on (hypothetical) (hypothetical)))

;;; Indeed, voltage divider needs to win here

(plunk! (thing '(potential B)))

(cpp (inquire (thing '(potential B))))
#;
((potential B) (has-value (*the-nothing*)))

;;; Hmmm... still not winning here.

(cpp (inquire (thing '(active Q))))
#;
((active Q) (has-value #f)
 (because
  ((function-propagator
    (#[compiled-closure 38 ...])
    ((-u
      (choose-exactly-one (active Q)
			  (cutoff Q)
			  (saturated Q))
      Q))
    (active Q))
   (choose-exactly-one (active Q)
		       (cutoff Q)
		       (saturated Q))
   Q))
 (depends-on (hypothetical)))

(cpp (inquire (thing '(saturated Q))))
#;
((saturated Q) (has-value #t)
 (because ((binary-amb (saturated Q)) Q))
 (depends-on (hypothetical)))

;;; Yup!

(cpp (inquire (thing '(cutoff Q))))
#;
((cutoff Q) (has-value #f)
 (because
  ((function-propagator
    (#[compiled-closure 38 ...])
    ((-v
      (choose-exactly-one (active Q)
			  (cutoff Q)
			  (saturated Q))
      Q))
    (cutoff Q))
   (choose-exactly-one (active Q)
		       (cutoff Q)
		       (saturated Q))
   Q))
 (depends-on (hypothetical)))

(cpp (inquire (thing '(current c Q))))
#;
((current c Q) (has-value (*the-nothing*)))

(plunk! (thing '(current c Q)))

(cpp (inquire (thing '(current c Q))))
#;
((current c Q)
 (has-value 9.247553600879604e-4)
 (because
  (solver)
  ((+ ((current t2 RB1) (current b Q)) (a B))
   (sum (current t2 RB1) (current b Q) (a B))
   B)
  ((- ((a B) (current t1 RB2)) (a B))
   (sum (a B) (current t1 RB2) (a B))
   B))
 (depends-on (gjs4)
             (gjs6)
             (gjs3)
             (gjs1)
             (gjs2)
             (gjs8)
             (gjs7)
             (hypothetical)))

(cpp (inquire (thing '(current b Q))))
#;
((current b Q)
 (has-value 3.914238592633108e-6)
 (because
  ((- ((i12 Q) (current c Q)) (current b Q))
   (sum (current b Q) (current c Q) (i12 Q))
   Q))
 (depends-on (gjs8)
             (gjs4)
             (gjs6)
             (gjs3)
             (gjs7)
             (hypothetical)
             (gjs2)
             (gjs1)))

(cpp (inquire (thing '(potential C))))
#;
((potential C)
 (has-value 1.1286695986805935)
 (because
  ((- ((potential TOP) (v RC)) (potential C))
   (sum (v RC) (potential C) (potential TOP))
   RC))
 (depends-on (gjs8)
             (gjs4)
             (gjs6)
             (gjs3)
             (gjs7)
             (hypothetical)
             (gjs2)
             (gjs1)))

(retract! 'gjs3)

(tell! RB2 1000 'gjs9)


(cpp (inquire (thing '(cutoff Q))))
#;
((cutoff Q) (has-value #f)
 (because
  ((function-propagator (#[compiled-closure 22 ...])
    ((-v
      (choose-exactly-one (active Q) (cutoff Q) (saturated Q))
      Q))
    (cutoff Q))
   (choose-exactly-one (active Q) (cutoff Q) (saturated Q))
   Q))
 (depends-on (hypothetical)))


;;;!!!  Wrong

(cpp (inquire (thing '(active Q))))
#;
((active Q) (has-value #f)
 (because
  ((function-propagator (#[compiled-closure 22 ...])
    ((-u
      (choose-exactly-one (active Q) (cutoff Q) (saturated Q))
      Q))
    (active Q))
   (choose-exactly-one (active Q) (cutoff Q) (saturated Q))
   Q))
 (depends-on (hypothetical)))

(cpp (inquire (thing '(saturated Q))))
#;
((saturated Q) (has-value #t)
               (because ((binary-amb (saturated Q)) Q))
               (depends-on (hypothetical)))
|#

(cpp (inquire (thing '(potential C))))
#;
((potential C) (has-value (*the-nothing*)))

(plunk! (thing '(potential C)))

(cpp (inquire (thing '(potential C))))
#;
((potential C)
 (has-value 15)
 (because
  ((- ((potential TOP) (v RC)) (potential C))
   (sum (v RC) (potential C) (potential TOP))
   RC))
 (depends-on (hypothetical) (hypothetical) (gjs2) (gjs1)))

;;; Yup!

(cpp (inquire (thing '(cutoff Q))))
#;
((cutoff Q)
 (has-value #t)
 (because
  ((function-propagator
    (#[compiled-closure 26 ...])
    ((u+v (choose-exactly-one (active Q) (cutoff Q) (saturated Q)) Q)
     (active Q))
    (cutoff Q))
   (disjunction
    (active Q)
    (cutoff Q)
    (u+v (choose-exactly-one (active Q) (cutoff Q) (saturated Q)) Q))
   (choose-exactly-one (active Q) (cutoff Q) (saturated Q))
   Q))
 (depends-on (hypothetical) (hypothetical)))

;;; Yay!