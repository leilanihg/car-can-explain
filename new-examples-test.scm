;;; ----------------------------------------------------------------------
;;; Copyright 2008 Alexey Radul and Gerald Jay Sussman.
;;; ----------------------------------------------------------------------
;;; This file is part of Artistic Propagator Prototype.
;;; 
;;; Artistic Propagator Prototype is free software; you can
;;; redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software
;;; Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;; 
;;; Artistic Propagator Prototype is distributed in the hope that it
;;; will be useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Artistic Propagator Prototype.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; This file contains the set of examples and necessary functions to
;;; run all of the code from the Art of the Propogator paper.
;;;
;;; All the interval code from the Art of the Propogator paper is in
;;; this file.  
;;; ----------------------------------------------------------------------

(declare (usual-integrations make-cell))

(define (heron-step x g h)
  (compound-propagator (list x g) h     ;inputs, output
    (lambda ()                          ;how to build
      (let-cells (x/g g+x/g (two 2))
        (divider x g x/g)
        (adder g x/g g+x/g)
        (divider g+x/g two h)))
    ;; given name for the compound propagator
    'heron-step))

#|
(initialize-scheduler)

(define-cell x)
(define-cell guess)
(define-cell better-guess)

(heron-step x guess better-guess)

(add-content x 2)
(add-content guess 1.4)
(run)

;;; Simplest system gives this
(cpp (content better-guess))
#;
1.4142857142857141

;;; But with dependencies (support and reasons)
(cpp (content better-guess))
#;
(supported 1.4142857142857141 () (#[compound-procedure 53 me]))

;;; Printed more nicely
(cpp (inquire better-guess))
#;
((better-guess)
 (has-value 1.4142857142857141)
 (because
  ((/ ((g+x/g heron-step) (two heron-step))
      (better-guess))
   heron-step))
 (depends-on))
|#

(define (sqrt-network x answer)
  (compound-propagator x answer
    (lambda ()
      (let-cells ((one 1.))
        (sqrt-iter x one answer)))
    'sqrt-network))

(define (sqrt-iter x g answer)
  (compound-propagator (list x g) answer
    (lambda ()
      (let-cells (done
		  not-done
		  x-if-not-done
		  g-if-not-done
		  new-g
                  ;;better-answer
                  )
        (good-enuf? g x done)
        (switch done g answer)
        ;;(spdt-switch done g better-answer answer)
        (inverter done not-done)
        (spst-switch not-done x x-if-not-done)
        (spst-switch not-done g g-if-not-done)
        (heron-step x-if-not-done g-if-not-done new-g)
        (sqrt-iter x-if-not-done new-g answer)
        ;;(sqrt-iter x-if-not-done new-g better-answer)
        ))
    'sqrt-iter))

(define (good-enuf? g x done)
  (compound-propagator (list g x) done
                       (lambda ()
                         (let-cells (g^2 (eps 0.00000001) x-g^2 ax-g^2)
                                    (multiplier g g g^2)
                                    (subtractor x g^2 x-g^2)
                                    (absolute-value x-g^2 ax-g^2)
                                    (<? ax-g^2 eps done)))
                       'good-enuf?))

#|
(initialize-scheduler)

(define-cell x)
(define-cell answer)

(sqrt-network x answer)

(add-content x 2)
(run)
(cpp (content answer))
#; 1.4142135623746899
|#

;;; Section Partial Information

(define (fall-duration t h)
  (compound-propagator t h
    (lambda ()
      (let-cells ((g (make-interval 9.789 9.832)) (one-half 1/2) t^2 gt^2)
        (c:square t t^2)
        (multiplier g t^2 gt^2)
        (multiplier one-half gt^2 h)))))

#|
(initialize-scheduler)

(define-cell fall-time)
(define-cell building-height)
(fall-duration fall-time building-height)

(add-content fall-time (make-interval 2.9 3.1))
(run)
(cpp (content building-height))
#; #(interval 41.163 47.243)
|#

(define (similar-triangles s-ba h-ba s h)
  (compound-propagator (list s-ba h-ba s) h
    (lambda ()
      (let-cells (ratio)
        (divider h-ba s-ba ratio)
        (multiplier s ratio h)))))

#|                                                                          
;;; Adding names to cells

(initialize-scheduler)

(define-cell barometer-height)
(define-cell barometer-shadow)                                       
(define-cell building-height)
(define-cell building-shadow)
(similar-triangles barometer-shadow barometer-height                        
                   building-shadow building-height)
                                                                            
(add-content building-shadow (make-interval 54.9 55.1))
(add-content barometer-height (make-interval 0.3 0.32))
(add-content barometer-shadow (make-interval 0.36 0.37))                    
(run)
(cpp (content building-height))
#; #(interval 44.514 48.978)                                                
                                                                            
(define-cell fall-time)                                              
(define-cell bh)

(fall-duration fall-time building-height)

(add-content fall-time (make-interval 2.9 3.1))                             
(run)
(cpp (content building-height))
#; #(interval 44.514 47.243)                                                
|# 

;;; Section Multidirectional Computation

(define (fall-duration t h)
  (compound-propagator (list t h) (list t h)
    (lambda ()
      (let-cells ((g (make-interval 9.789 9.832)) (one-half 1/2) t^2 gt^2)
        (quadratic t t^2)
        (product g t^2 gt^2)
        (product one-half gt^2 h)))
    `(fall-duration ,(name t) ,(name h))))

(define (similar-triangles s-ba h-ba s h)
  (compound-propagator (list s-ba h-ba s h)
		       (list s-ba h-ba s h)
    (lambda ()
      (let-cells (ratio)
        (product s-ba ratio h-ba)
        (product s ratio h)))
    `(similar-triangles ,(name s-ba)
			,(name h-ba)
			,(name s)
			,(name h))))

#|
(initialize-scheduler)

(define-cell barometer-height)
(define-cell barometer-shadow)
(define-cell building-height)
(define-cell building-shadow)
(similar-triangles barometer-shadow barometer-height
                   building-shadow building-height)

(add-content building-shadow (make-interval 54.9 55.1))
(add-content barometer-height (make-interval 0.3 0.32))
(add-content barometer-shadow (make-interval 0.36 0.37))
(run)
(cpp (content building-height))
#; #(interval 44.514 48.978)

(define-cell fall-time)
(fall-duration fall-time building-height)

(add-content fall-time (make-interval 2.9 3.1))
(run)
(cpp (content building-height))
#; #(interval 44.514 47.243)

(cpp (content barometer-height))
#; #(interval .3 .31839)

(cpp (content fall-time))
#; #(interval 3.0091 3.1)
;;; So fall-time improved because of shadow info

(add-content building-height (make-interval 45 45))
(run)
(cpp (content barometer-height))
#; #(interval .3 .30328)

(cpp (content barometer-shadow))
#; #(interval .366 .37)

(cpp (content building-shadow))
#; #(interval 54.9 55.1)

(cpp (content fall-time))
#; #(interval 3.0255 3.0322)
|#

;;; Section Generic Operations

#|
(initialize-scheduler)

(define-cell fall-time)
(define-cell building-height)
(fall-duration fall-time building-height)

(add-content fall-time (make-interval 2.9 3.1))
(run)
(cpp (content building-height))
#; #(interval 41.163 47.243)

(add-content building-height 45)

(run)
(cpp (content fall-time))
#; #(interval 3.0255 3.0322)
|#


;;; Section Dependencies

;;; Subsection Dependencies for Provenance

#|
(initialize-scheduler)

(define-cell barometer-height)
(define-cell barometer-shadow)
(define-cell building-height)
(define-cell building-shadow)
(similar-triangles barometer-shadow barometer-height
                   building-shadow building-height)

(add-content building-shadow
 (supported (make-interval 54.9 55.1) '(shadows)))
(add-content barometer-height
 (supported (make-interval 0.3 0.32) '(shadows)))
(add-content barometer-shadow
 (supported (make-interval 0.36 0.37) '(shadows)))
(run)
(cpp (content building-height))
#;
#(supported #(interval 44.51351351351351 48.977777777777774)
	    (shadows)
	    (me))

#|
(cpp (name (car (v&s-reasons (content building-height)))))
#;
((*
  ((building-shadow)
   (ratio
    (similar-triangles (barometer-shadow)
		       (barometer-height)
		       (building-shadow)
		       (building-height))))
  (building-height))
 (product
  (building-shadow)
  (ratio
   (similar-triangles (barometer-shadow)
		      (barometer-height)
		      (building-shadow)
		      (building-height)))
  (building-height))
 (similar-triangles (barometer-shadow)
		    (barometer-height)
		    (building-shadow)
		    (building-height)))
|#

(define-cell fall-time)
(fall-duration fall-time building-height)

(add-content fall-time
 (supported (make-interval 2.9 3.3) '(lousy-fall-time)))
(run)
(cpp (content building-height))
#; 
#(supported #(interval 44.51351351351351 48.977777777777774)
	    (shadows)
	    (#[compound-procedure 160 me]))

(add-content fall-time
 (supported (make-interval 2.9 3.1) '(better-fall-time)))
(run)
(cpp (content building-height))
#;
#(supported #(interval 44.51351351351351 47.24276000000001)
	    (better-fall-time shadows)
	    (#[compound-procedure 164 me]))

(add-content building-height (supported 45 '(superintendent)))
(run)
(cpp (content building-height))
#; #(supported 45 (superintendent) ((*universal-ancestor*)))

(cpp (content barometer-height))
#; 
#(supported #(interval .3 .30327868852459017)
	    (superintendent better-fall-time shadows)
	    (#[compound-procedure 167 me]
	     (*universal-ancestor*)))

(cpp (content barometer-shadow))
#; 
#(supported #(interval .366 .37)
	    (better-fall-time superintendent shadows)
	    (#[compound-procedure 169 me] (*universal-ancestor*)))

(cpp (content building-shadow))
#; 
#(supported #(interval 54.9 55.1)
	    (shadows)
	    ((*universal-ancestor*)))

(cpp (content fall-time))
#;
#(supported #(interval 3.025522031629098 3.0321598338046556)
	    (superintendent)		; (shadows superintendent) error in paper.
	    (#[compound-procedure 172 me]))
|#

;;; Subsection Dependencies for Alternate Worldviews
#|
(initialize-scheduler)

(define-cell barometer-height)
(define-cell barometer-shadow)
(define-cell building-height)
(define-cell building-shadow)
(similar-triangles barometer-shadow barometer-height
                   building-shadow building-height)

(add-content building-shadow
 (make-tms (supported (make-interval 54.9 55.1) '(shadows))))
(add-content barometer-height
 (make-tms (supported (make-interval 0.3 0.32) '(shadows))))
(add-content barometer-shadow
 (make-tms (supported (make-interval 0.36 0.37) '(shadows))))
(run)
(cpp (content building-height))
#; 
(tms
 ((supported (interval 44.51351351351351 48.977777777777774)
             (shadows)
             (#[compound-procedure 176 ...]))))

(define-cell fall-time)
(fall-duration fall-time building-height)

(add-content fall-time
 (make-tms (supported (make-interval 2.9 3.1) '(fall-time))))
(run)
(cpp (content building-height))
#;
(tms
 ((supported (interval 44.51351351351351 47.24276000000001)
             (fall-time shadows)
             (#[compound-procedure 105 ...]))
  (supported (interval 44.51351351351351 48.977777777777774)
             (shadows)
             (#[compound-procedure 103 ...]))))

(cpp (tms-query (content building-height)))
#;
(supported (interval 44.51351351351351 48.977777777777774)
           (shadows)
           (#[compound-procedure 103 me]))

(kick-out! 'fall-time)
(run)
(cpp (tms-query (content building-height)))
#;
(supported (interval 44.51351351351351 48.977777777777774)
           (shadows)
           (#[compound-procedure 103 me]))

#|
(cpp (map name (v&s-reasons (tms-query (content building-height)))))
#;
(((* ((building-shadow)
      (ratio (similar-triangles (barometer-shadow)
                                (barometer-height)
                                (building-shadow)
                                (building-height))))
     (building-height))
  (product (building-shadow)
           (ratio (similar-triangles (barometer-shadow)
                                     (barometer-height)
                                     (building-shadow)
                                     (building-height)))
           (building-height))
  (similar-triangles (barometer-shadow)
                     (barometer-height)
                     (building-shadow)
                     (building-height))))

;;; By contrast...
|#

(bring-in! 'fall-time)
(kick-out! 'shadows)
(run)
(cpp (tms-query (content building-height)))
#; 
(supported (interval 41.162745 47.24276000000001)
           (fall-time)
           (#[compound-procedure 105 me]))

(cpp (content building-height))
#;
(tms
 ((supported (interval 41.162745 47.24276000000001)
             (fall-time)
             (#[compound-procedure 105 ...]))
  (supported (interval 44.51351351351351 47.24276000000001)
             (fall-time shadows)
             (#[compound-procedure 105 ...]))
  (supported (interval 44.51351351351351 48.977777777777774)
             (shadows)
             (#[compound-procedure 103 ...]))))

(add-content building-height
             (supported 45 '(superintendent)))

(run)
(cpp (content building-height))
#; 
(tms
 ((supported 45 (superintendent) ((*universal-ancestor*)))
  (supported (interval 41.162745 47.24276000000001)
             (fall-time)
             (#[compound-procedure 105 ...]))
  (supported (interval 44.51351351351351 47.24276000000001)
             (fall-time shadows)
             (#[compound-procedure 105 ...]))
  (supported (interval 44.51351351351351 48.977777777777774)
             (shadows)
             (#[compound-procedure 103 ...]))))

(cpp (tms-query (content building-height)))
#;
(supported 45 (superintendent) ((*universal-ancestor*)))

(bring-in! 'shadows)
(run)
(cpp (tms-query (content building-height)))
#;
(supported 45 (superintendent) ((*universal-ancestor*)))

(cpp (content barometer-height))
#;
(tms ((supported (interval .3 .32)
                 (shadows)
                 ((*universal-ancestor*)))))


(cpp (tms-query (content barometer-height)))
#; 
(supported (interval .3 .32) (shadows) ((*universal-ancestor*)))

(kick-out! 'fall-time)
(run)
(cpp (tms-query (content barometer-height)))
#; 
(supported (interval .3 .32) (shadows) ((*universal-ancestor*)))

(bring-in! 'fall-time)
(run)
(cpp (tms-query (content barometer-height)))
#; 
(supported (interval .3 .30327868852459017)
           (superintendent shadows)
           ((*universal-ancestor*) #[compound-procedure 137 me]))

(cpp (content barometer-height))
#;
(tms
 ((supported (interval .3 .30327868852459017)
             (superintendent shadows)
             ((*universal-ancestor*) #[compound-procedure 137 ...]))
  (supported (interval .3 .3183938287795994)
             (fall-time shadows)
             ((*universal-ancestor*) #[compound-procedure 137 ...]))
  (supported (interval .3 .32) (shadows) ((*universal-ancestor*)))))

(add-content building-height
  (supported (make-interval 46. 50.) '(pressure)))
(run)
#; 
(contradiction #[compound-procedure 51 me]
	       (superintendent pressure)
	       ((*universal-ancestor*)))

(cpp (tms-query (content building-height)))
#; 
(supported (contradiction) 
           (superintendent pressure)
           ((*universal-ancestor*)))

(cpp (tms-query (content barometer-height)))
#;
(supported (interval .3 .30327868852459017)
           (superintendent shadows)
           ((*universal-ancestor*) #[compound-procedure 179 me]))

(kick-out! 'superintendent)
(run)
(cpp (tms-query (content building-height)))
#; 
(supported (interval 46. 47.24276000000001)
           (fall-time pressure)
           (#[compound-procedure 49 me] (*universal-ancestor*)))

(cpp (tms-query (content barometer-height)))
#; 
(supported (interval .3005444646098004 .3183938287795994)
           (pressure fall-time shadows)
           (#[compound-procedure 179 me]))

(bring-in! 'superintendent)
(kick-out! 'pressure)
(run)
(cpp (tms-query (content building-height)))
#; 
(supported 45
           (superintendent)
           ((*universal-ancestor*)))

(cpp (tms-query (content barometer-height)))
#; 
(supported (interval .3 .30327868852459017)
           (superintendent shadows)
           ((*universal-ancestor*) #[compound-procedure 179 me]))

(cpp (explain barometer-height))
#;
(((barometer-height)
  (has-value (interval .3 .30327868852459017))
  (because
   ((*
     ((barometer-shadow)
      (ratio
       (similar-triangles (barometer-shadow) (barometer-height) (building-shadow) (building-height))))
     (barometer-height))
    (product
     (barometer-shadow)
     (ratio
      (similar-triangles (barometer-shadow) (barometer-height) (building-shadow) (building-height)))
     (barometer-height))
    (similar-triangles (barometer-shadow) (barometer-height) (building-shadow) (building-height))))
  (depends-on (superintendent) (shadows)))
 ((barometer-shadow)
  (has-value (interval .366 .37))
  (because
   ((/
     ((barometer-height)
      (ratio
       (similar-triangles (barometer-shadow) (barometer-height) (building-shadow) (building-height))))
     (barometer-shadow))
    (product
     (barometer-shadow)
     (ratio
      (similar-triangles (barometer-shadow) (barometer-height) (building-shadow) (building-height)))
     (barometer-height))
    (similar-triangles (barometer-shadow) (barometer-height) (building-shadow) (building-height))))
  (depends-on (superintendent) (shadows)))
 ((ratio similar-triangles)
  (has-value (interval .8166969147005445 .819672131147541))
  (because
   ((/
     ((building-height) (building-shadow))
     (ratio
      (similar-triangles (barometer-shadow) (barometer-height) (building-shadow) (building-height))))
    (product
     (building-shadow)
     (ratio
      (similar-triangles (barometer-shadow) (barometer-height) (building-shadow) (building-height)))
     (building-height))
    (similar-triangles (barometer-shadow) (barometer-height) (building-shadow) (building-height))))
  (depends-on (shadows) (superintendent)))
 ((building-height) (has-value 45) (because) (depends-on (superintendent)))
 ((building-shadow) (has-value (interval 54.9 55.1)) (because) (depends-on (shadows))))
|#

;;; Subsection Dependencies for Implicit Search

#|
(define (multiple-dwelling)
  (let-cells (baker  cooper fletcher  miller smith)
    (let ((floors '(1 2 3 4 5)))
      (one-of floors baker)       (one-of floors cooper)
      (one-of floors fletcher)    (one-of floors miller)
      (one-of floors smith)
      (require-distinct
       (list baker cooper fletcher miller smith))
      (let-cells (b=5    c=1   f=5    f=1
		  m>c    sf    fc     one
		  five   s-f   as-f   f-c
		  af-c)
	((constant 1) one)        ((constant 5) five)
	(=? five baker b=5)       (abhor b=5)
	(=? one cooper c=1)       (abhor c=1)
	(=? five fletcher f=5)    (abhor f=5)
	(=? one fletcher f=1)     (abhor f=1)
	(>? miller cooper m>c)    (require m>c)
	(subtractor smith fletcher s-f)
	(absolute-value s-f as-f)
	(=? one as-f sf)          (abhor sf)
	(subtractor fletcher cooper f-c)
	(absolute-value f-c af-c)
	(=? one af-c fc)          (abhor fc)
	(list baker cooper fletcher miller smith)))))

(define (multiple-dwelling)
  (let-cells (baker  cooper fletcher  miller smith)
    (let ((floors '(1 2 3 4 5)))
      (one-of floors baker)       (one-of floors cooper)
      (one-of floors fletcher)    (one-of floors miller)
      (one-of floors smith)
      (require-distinct
       (list baker cooper fletcher miller smith))
      (let-cells (b=5    c=1   f=5    f=1
		  m>c    sf    fc     one
		  five   s-f   as-f   f-c
		  af-c)
	((constant 1) one)        ((constant 5) five)
	(=? five baker b=5)       (abhor b=5)
	(=? one cooper c=1)       (abhor c=1)
	(=? five fletcher f=5)    (abhor f=5)
	(=? one fletcher f=1)     (abhor f=1)
	(>? miller cooper m>c)    (require m>c)
	(sum fletcher s-f smith)
	(absolute-value s-f as-f)
	(=? one as-f sf)          (abhor sf)
	(sum cooper f-c fletcher)
	(absolute-value f-c af-c)
	(=? one af-c fc)          (abhor fc)
	(list baker cooper fletcher miller smith)))))
|#

(define (multiple-dwelling)
  (let-cells (baker  cooper fletcher  miller smith)
    (let ((floors '(1 2 3 4 5)))
      (one-of floors baker)       (one-of floors cooper)
      (one-of floors fletcher)    (one-of floors miller)
      (one-of floors smith)
      (require-distinct
       (list baker cooper fletcher miller smith))
      (let-cells (b=5        c=1   f=5    f=1
		  m>c        sf    fc     (one 1)
		  (five 5)   s-f   as-f   f-c
		  af-c)
	(=? five baker b=5)       (abhor b=5)
	(=? one cooper c=1)       (abhor c=1)
	(=? five fletcher f=5)    (abhor f=5)
	(=? one fletcher f=1)     (abhor f=1)
	(>? miller cooper m>c)    (require m>c)
	(sum fletcher s-f smith)
	(+->+ s-f as-f)
	(=? one as-f sf)          (abhor sf)
	(sum cooper f-c fletcher)
	(+->+ f-c af-c)
	(=? one af-c fc)          (abhor fc)
	(list baker cooper fletcher miller smith)))))

#|
(initialize-scheduler)

(define answers (multiple-dwelling))
(run)
(map (compose v&s-value tms-query content) answers)
#; (3 2 4 5 1)

*number-of-calls-to-fail*
#; 73

(cpp (inquire (car answers)))
#;
((baker)
 (has-value 3)
 (because
  ((conditional (amb-cell-from-one-of-1)
                (cell)
                (link-cell-from-one-of-1)
                (baker))))
 (depends-on (hypothetical false (amb-cell-from-one-of-2))
             (hypothetical true (amb-cell-from-one-of-3))
             (hypothetical false (amb-cell-from-one-of-1))))
|#

#|
;;; A little expression compilation would allow

(define (multiple-dwelling)
  (let-cells ((baker (one-of 1 2 3 4 5))
	      (cooper (one-of 1 2 3 4 5))
              (fletcher (one-of 1 2 3 4 5))
	      (miller (one-of 1 2 3 4 5))
	      (smith (one-of 1 2 3 4 5)))
    (require-distinct
     (list baker cooper fletcher miller smith))
    (abhor (= baker 5))     (abhor (= cooper 1))
    (abhor (= fletcher 5))  (abhor (= fletcher 1))
    (require (> miller cooper))
    (abhor (= 1 (abs (- smith fletcher))))
    (abhor (= 1 (abs (- fletcher cooper))))
    (list baker cooper fletcher miller smith)))

|#

