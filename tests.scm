
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
((c) (has-value (interval 3 5))
     (because ((+ ((a) (b)) (c)) (sum (a) (b) (c))))
     (depends-on (gjs2) (gjs1)))

;;; No change, because the merge in cell e of (2,5) and (1,2.5) did
;;; not improve the bounds on the value in e, even though this could
;;; have improved the bounds on the value in c!


     
