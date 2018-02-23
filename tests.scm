
(initialize-scheduler)

(define-cell a)

(define-cell b)

(define-cell c)

(define-cell d)

(define-cell e)

(define a1 (adder a b c))

(define m1 (multiplier d e c))

(tell! a (make-interval 1 2) 'gjs1)

(cpp (inquire a))
#;
((a) (has-value #(interval 1 2))
     (because ())
     (depends-on (gjs1)))


(tell! b (make-interval 2 3) 'gjs2)

(cpp (inquire c))
#;
((c) (has-value #(interval 3 5))
     (because ((+ ((a) (b)) (c))))
     (depends-on (gjs2) (gjs1)))

(tell! d (make-interval 2 3) 'gjs3)

(tell! e (make-interval 2 5) 'gjs4)

(cpp (inquire c))
#;
((c) (has-value #(interval 4 5))
     (because  ((+ ((a) (b)) (c))) ((* ((d) (e)) (c))))
     (depends-on (gjs1) (gjs2) (gjs4) (gjs3)))


(cpp (explain c))
#;
(((c) (has-value #(interval 4 5))
      (because ((+ ((a) (b)) (c))) ((* ((d) (e)) (c))))
      (depends-on (gjs1) (gjs2) (gjs4) (gjs3)))
 ((a) (has-value #(interval 1 2))
      (because ())
      (depends-on (gjs1)))
 ((b) (has-value #(interval 2 3))
      (because ())
      (depends-on (gjs2)))
 ((d) (has-value #(interval 2 3))
      (because ())
      (depends-on (gjs3)))
 ((e) (has-value #(interval 2 5))
      (because ())
      (depends-on (gjs4))))

;;; -------------------------------------------------------


(initialize-scheduler)

(define-cell a)

(define-cell b)

(define-cell c)

(define-cell d)

(define-cell e)

(define a1 (sum a b c))

(define m1 (product d e c))

(tell! a (make-interval 1 2) 'gjs1)

(cpp (inquire a))
#;
((a) (has-value #(interval 1 2))
     (because ())
     (depends-on (gjs1)))

(tell! b (make-interval 2 3) 'gjs2)

(cpp (inquire c))
#;
((c) (has-value #(interval 3 5))
     (because ((+ ((a) (b)) (c)) (sum (a) (b) (c))))
     (depends-on (gjs2) (gjs1)))

(tell! d (make-interval 2 3) 'gjs3)

(cpp (inquire e))
#;
((e) (has-value #(interval 1. 2.5))
     (because ((/ ((c) (d)) (e)) (product (d) (e) (c))))
     (depends-on (gjs3) (gjs2) (gjs1)))

(cpp (explain e))
#;
(((e) (has-value #(interval 1. 2.5))
      (because ((/ ((c) (d)) (e)) (product (d) (e) (c))))
      (depends-on (gjs3) (gjs2) (gjs1)))
 ((c) (has-value #(interval 3 5))
      (because ((+ ((a) (b)) (c)) (sum (a) (b) (c))))
      (depends-on (gjs2) (gjs1)))
 ((a) (has-value #(interval 1 2))
      (because ())
      (depends-on (gjs1)))
 ((b) (has-value #(interval 2 3))
      (because ())
      (depends-on (gjs2)))
 ((d) (has-value #(interval 2 3))
      (because ())
      (depends-on (gjs3))))


(tell! e (make-interval 2 5) 'gjs4)

(cpp (inquire c))
#;
((c) (has-value #(interval 4. 5.))
     (because ((+ ((a) (b)) (c))
	       (sum (a) (b) (c)))
	      ((* ((d) (e)) (c))
	       (product (d) (e) (c))))
     (depends-on (gjs4) (gjs2) (gjs1) (gjs3)))

(cpp (explain c))
#;
(((c) (has-value (interval 4. 5.))
      (because ((+ ((a) (b)) (c)) (sum (a) (b) (c)))
	       ((* ((d) (e)) (c)) (product (d) (e) (c))))
      (depends-on (gjs4) (gjs2) (gjs1) (gjs3)))
 ((a) (has-value (interval 1 2)) (because ()) (depends-on (gjs1)))
 ((b) (has-value (interval 2 3)) (because ()) (depends-on (gjs2)))
 ((e) (has-value (interval 2. 2.5))
      (because ((/ ((c) (d)) (e)) (product (d) (e) (c))) ())
      (depends-on (gjs1) (gjs2) (gjs3) (gjs4)))
 ((d) (has-value (interval 2. 2.5))
      (because () ((/ ((c) (e)) (d)) (product (d) (e) (c))))
      (depends-on (gjs4) (gjs2) (gjs1) (gjs3))))

;;; Note: b,e,d have changed to make system consistent.  Cell a is
;;; unchanged.  Probably should be able to tie some stuff down!

(cpp (explain b))
#;
(((b) (has-value (interval 2 3)) (because ()) (depends-on (gjs2))))


