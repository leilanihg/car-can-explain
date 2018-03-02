;;; ----------------------------------------------------------------------
;;; Copyright 2016 Leilani H Gilpin, Ben Z Yuan and Gerald Jay Sussuman 
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

;; Tire information 
(define (diameter tire)
  (tire 'diameter))

(define (pressure tire)
  (tire 'pressure))

(define (rotation tire)
  (tire 'rotation))

(define (friction-force tire)
  (tire 'friction-force))

(define (normal-force tire)
  (tire 'normal-force))

;; center of mass informatio
(define (lateral-relative-pressure com)
  (com 'lateral-relative-pressure))

(define (longitudinal-relative-pressure com)
  (com 'longitudinal-relative-pressure))

(define (brake-pressure com)
  (com 'brake-pressure))

(define (steering-angle com)
  (com 'steering-angle))

; May need to have more brake information
(define (friction brake)
  (brake 'friction))

; Cylinder information
(define (hydraulic cylinder)
  (cylinder 'hydraulic))

(define (booster cylinder)
  (cylinder 'booster))

(define (foot-pressure cylinder)
  (cylinder 'foot-pressure))

(define (front-pressure cylinder)
  (cylinder 'front-pressure))

(define (back-pressure cylinder)
  (cylinder 'back-pressure))

; Disk information
(define (caliper disk)
  (disk 'caliper))

(define (rotor disk)
  (disk 'rotor))

(define (brake-pad disk)
  (disk 'brake-pad))

(define (friction disk)
  (disk 'friction))

; GPS information
(define (heading gps)
  (gps 'heading))

(define (speed gps)
  (gps 'speed))

; Steering information
(define (direction steering)
  (steering 'direction))

(define (heading steering)
  (steering 'heading))

; Throttle Information
(define (gas throttle)
  (throttle 'gas))