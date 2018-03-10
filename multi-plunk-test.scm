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

(cpp (inquire (thing '(potential n3))))
#;
((potential n3) (has-value (*the-nothing*))
		(because ())
		(depends-on))

(plunk! (thing '(potential n3)))
(plunk! (thing '(potential n2)))

(cpp (inquire (thing '(potential n3))))
#;
((potential n3)
 (has-value 50/37)
 (because
  ((- ((potential n1) (v R3)) (potential n3))
   (sum (v R3) (potential n3) (potential n1))
   R3))
 (depends-on (gjs5)
             (gjs7)
             (gjs9)
             (gjs2)
             (premise-potentialn3_1)
             (gjs8)
             (gjs6)
             (gjs1)
             (gjs3)
             (gjs4)))

(cpp (inquire (thing '(potential n2))))
#;
((potential n2)
 (has-value 60/37)
 (because
  ((/ ((v R2) (rR2)) (current t1 R2)) (product (rR2) (current t1 R2) (v R2))
                                      R2)
  ((- ((ie_44 n1) (current t2 R1)) (current t1 R2))
   (sum (current t1 R2) (current t2 R1) (ie_44 n1))
   n1)
  ((solver))
  ((- ((potential n1) (potential n3)) (v R3))
   (sum (v R3) (potential n3) (potential n1))
   R3)
  ((* ((rR3) (current t1 R3)) (v R3)) (product (rR3) (current t1 R3) (v R3))
                                      R3))
 (depends-on (gjs4)
             (gjs3)
             (gjs1)
             (gjs6)
             (gjs8)
             (premise-potentialn3_1)
             (gjs2)
             (gjs9)
             (gjs7)
             (gjs5)))

(cpp (inquire (thing '(potential n1))))
#;
((potential n1)
 (has-value 130/37)
 (because
  ((+ ((v R2) (potential gnd)) (potential n1))
   (sum (v R2) (potential gnd) (potential n1))
   R2))
 (depends-on (gjs5)
             (gjs7)
             (gjs9)
             (gjs2)
             (premise-potentialn3_1)
             (gjs8)
             (gjs6)
             (gjs1)
             (gjs3)
             (gjs4)))


(cpp (content (thing '(potential n3))))
#;
(tms
 ((supported 50/37
             (gjs5 gjs7 gjs9 gjs2 premise-potentialn3_1 gjs8 gjs6 gjs1 gjs3 gjs4)
             (#[compound-procedure 46 ...]))
  (supported
   (*number* (expression (* 5/6 potentialn2_2)))
   (gjs6 gjs8 premise-potentialn3_1 gjs2 gjs9 gjs7 premise-potentialn2_2 gjs5)
   (#[compound-procedure 46 ...] (solver)
                                 #[compound-procedure 45 ...]
                                 #[compound-procedure 44 ...]))))


(cpp (content (thing '(potential n2))))
#;
(supported
 60/37
 (gjs4 gjs3 gjs1 gjs6 gjs8 premise-potentialn3_1 gjs2 gjs9 gjs7 gjs5)
 (#[compound-procedure 48 me] #[compound-procedure 47 me]
                              (solver)
                              #[compound-procedure 45 me]
                              #[compound-procedure 44 me]))


(cpp (content (thing '(potential n1))))
#;
(tms
 ((supported 130/37
             (gjs5 gjs7 gjs9 gjs2 premise-potentialn3_1 gjs8 gjs6 gjs1 gjs3 gjs4)
             (#[compound-procedure 50 ...]))
  (supported
   (*number* (expression (* 13/6 potentialn2_2)))
   (gjs6 gjs8 premise-potentialn3_1 gjs2 gjs9 gjs7 premise-potentialn2_2 gjs5)
   (#[compound-procedure 49 ...] (solver)
                                 #[compound-procedure 45 ...]
                                 #[compound-procedure 44 ...]))))

;;; Could not flush premises because n1 and n3 have values in their
;;; tmss that depend on the potential2_2 that we must get from
;;; substitution from n2.  But the tmss also have the numerical values
;;; also.  Perhaps all cells with expressions that have plunk
;;; variables should save a list and know to flush them when they are
;;; determined as constants, thus killing off the plunk premises??
