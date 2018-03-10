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
;;(tell! RB1 162000 'gjs4)
(tell! RC  4990   'gjs5)
(tell! RE  1000   'gjs6)

(tell! VThreshold 0.65 'gjs7)
(tell! VSaturation 0.2 'gjs7)

(tell! (potential C) (make-interval 9 11) 'gjs8)

(cpp (inquire RB1))
#;
((rb1)
 (has-value #(interval 141951.6416941635 186668.04721474426))
 (because ((/ ((v rb1) (terminal-current t1 rb1)) (rb1))
	   (product (rb1) (terminal-current t1 rb1) (v rb1))
	   rb1))
 (depends-on (gjs3) (gjs7) (gjs8) (gjs5) (gjs6) (gjs2) (gjs1)))

