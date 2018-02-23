(initialize-scheduler)
;;width of the car in front
;;from sensor
(define-cell w)
;;distance from the front wheel to the front of your car
(define-cell k)
;;your car's wheel base
(define-cell l)
;;radius of your car's curb-to-curb turning circle
(define-cell r)
;;length of your car
(define-cell d)

(define-cell rlsqdiff (p:- (p:square r) (p:square l)))
(define-cell lksumsq (p:square (p:+ l k)))
(define-cell c (p:square (p:- (p:sqrt rlsqdiff) w)))
(define-cell sqrtres (p:sqrt(p:-(p:+ rlsqdiff lksumsq) (p:square (p:- c w)))))
;;minimum space needed
(define-cell m (p:+ d (p:- (p:- sqrtres l) k)))
(add-content w 1.5) ;;m, placeholder for lidar data
;;estimate based on l and d
(add-content k 0.8) ;;m
;;2008 Cadillac CTS
(add-content l 2.8804) ;;m
;;2008 Cadillac CTS
(add-content r 10.82) ;;m
;;2008 Cadillac CTS
(add-content d 4.8285) ;;m
(run)
(content m) ;;m