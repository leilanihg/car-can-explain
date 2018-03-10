
;;; Example usages of propagator networks

;;; Unidirectional Fahrenheit to Celsius conversion

 (define (fahrenheit->celsius f c)
   (compound-propagator (list f) c
     (lambda ()
       (let-cells (thirty-two f-32 five c*9 nine)
	  ((constant 32) thirty-two)
	  ((constant 5) five)
	  ((constant 9) nine)
	  (p:- f thirty-two f-32)
	  (p:* f-32 five c*9)
	  (p:/ c*9 nine c)))))

#|
 (initialize-scheduler)
 (define-cell f)
 (define-cell c)

 (fahrenheit->celsius f c)

 (add-content f 77)
 (run)
 (content c)
 ;Value: (supported 25 () (#[compound-procedure 69 me]))
|#

;;; Multidirectional Fahrenheit to Celsius to Kelvin conversion

 (define (fahrenheit-celsius f c)
   (constraint-propagator (list f c)
     (let-cells (f-32 c*9 thirty-two five nine)
       ((constant 32) thirty-two)
       ((constant 5) five)
       ((constant 9) nine)

       (c:+ thirty-two f-32 f)
       (c:* f-32 five c*9)
       (c:* c nine c*9))
     `(fahrenheit-celsius ,(name f) ,(name c))))

(define-propagator (c:celsius-kelvin c k)
  (c:+ c 273.15 k))

#|
(initialize-scheduler)
(define-cell f)
(define-cell c)

(fahrenheit-celsius f c)

(add-content c 25)
(run)
(content f)
;Value: 77

 (define-cell k)

 (c:celsius-kelvin c k)
 (run)
 (content k)
 ;Value: 298.15
|#
