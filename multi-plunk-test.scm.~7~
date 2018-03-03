;;;; A complicated network that requires 2 plunk variables.

(initialize-scheduler)

(define n0 (node 'n0))

(define n1 (node 'n1))

(define n2 (node 'n2))

(define n3 (node 'n3))

(define gnd (node 'gnd))

(define-cell V)
(define VS ((voltage-source V 'VS) n0 gnd))

(define-cell rR1)
(define R1 ((linear-resistor rR1 'R1) n0 n1))

(define-cell rR2)
(define R2 ((linear-resistor rR2 'R2) n1 gnd))

(define-cell rR3)
(define R3 ((linear-resistor rR3 'R3) n1 n3))

(define-cell rR4)
(define R4 ((linear-resistor rR4 'R4) n1 n2))

(define-cell rR5)
(define R5 ((linear-resistor rR5 'R5) n2 n3))

(define-cell rR6)
(define R6 ((linear-resistor rR6 'R6) n2 gnd))

(define-cell rR7)
(define R7 ((linear-resistor rR7 'R7) n3 gnd))

(cap! n0)
(cap! n1)
(cap! n2)
(cap! n3)
(cap! gnd)

(tell! V 10 'gjs1)
(tell! (thing '(potential gnd)) 0 'gjs2)

(tell! rR1 1000 'gjs3)
(tell! rR2 1000 'gjs4)
(tell! rR3 2000 'gjs5)
(tell! rR4 1000 'gjs6)
(tell! rR5 1000 'gjs7)
(tell! rR6 1000 'gjs8)
(tell! rR7 1000 'gjs9)

#|
(cpp (inquire (thing '(potential n3))))
#;
((potential n3) (has-value (*the-nothing*))
		(because ())
		(depends-on))

|#

(plunk! (thing '(potential n3)))
(plunk! (thing '(potential n2)))

(cpp (inquire (thing '(potential n3))))
#;
((potential n3)
 (has-value 50/37)
 (because (solver)
	  ((- ((a n1) (current t1 R2)) (current t2 R1))
	   (sum (current t1 R2) (current t2 R1) (a n1))
	   n1)
	  ((- ((zero-i R1) (current t1 R1)) (current t2 R1))
	   (sum (current t1 R1) (current t2 R1) (zero-i R1))
	   R1)
	  ((+ ((v R3) (potential n3)) (potential n1))
	   (sum (v R3) (potential n3) (potential n1))
	   R3)
	  ((+ ((v R4) (potential n2)) (potential n1))
	   (sum (v R4) (potential n2) (potential n1))
	   R4))
 (depends-on (gjs4) (gjs3) (gjs1) (gjs5) (gjs9)
	     (gjs2) (gjs8) (gjs7) (gjs6)))

(cpp (inquire (thing '(potential n2))))
#;
((potential n2) (has-value 60/37)
 (because ((- ((potential n2) (v R5)) (potential n3))
	   (sum (v R5) (potential n3) (potential n2))
	   R5)
	  (solver)
	  ((+ ((v R3) (potential n3)) (potential n1))
	   (sum (v R3) (potential n3) (potential n1))
	   R3)
	  ((+ ((v R4) (potential n2)) (potential n1))
	   (sum (v R4) (potential n2) (potential n1))
	   R4)
	  ((- ((zero-i R1) (current t1 R1)) (current t2 R1))
	   (sum (current t1 R1) (current t2 R1) (zero-i R1))
	   R1)
	  ((- ((a n1) (current t1 R2)) (current t2 R1))
	   (sum (current t1 R2) (current t2 R1) (a n1))
	   n1))
 (depends-on (gjs6) (gjs1) (gjs3) (gjs5) (gjs4)
	     (gjs9) (gjs7) (gjs8) (gjs2)))

(cpp (inquire (thing '(potential n1))))
#;
((potential n1) (has-value 130/37)
 (because ((+ ((v R4) (potential n2)) (potential n1))
	   (sum (v R4) (potential n2) (potential n1))
	   R4))
 (depends-on (gjs2) (gjs8) (gjs7) (gjs9) (gjs4)
	     (gjs5) (gjs3) (gjs1) (gjs6)))


