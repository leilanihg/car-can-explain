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

(environment-define user-initial-environment
		    'car-environment
		    (extend-top-level-environment user-initial-environment))

(environment-define car-environment 'foot-brake-pressure #f)
(environment-define car-environment 'front-brakes-pressure #f)
(environment-define car-environment 'back-brakes-pressure #f)

(environment-define car-environment 'left-back-pressure #f)
(environment-define car-environment 'right-back-pressure #f)
(environment-define car-environment 'left-front-pressure #f)
(environment-define car-environment 'right-front-pressure #f)

(environment-define car-environment 'left-back-tire #f)
(environment-define car-environment 'right-back-tire #f)
(environment-define car-environment 'left-front-tire #f)
(environment-define car-environment 'right-front-tire #f)

(environment-define car-environment 'brake-master #f)
(environment-define car-environment 'antilock-brakes #f)
(environment-define car-environment 'left-back-disk-brake #f)
(environment-define car-environment 'right-back-disk-brake #f)
(environment-define car-environment 'left-front-disk-brake #f)
(environment-define car-environment 'right-front-disk-brake #f)

(environment-define car-environment 'heading-cell #f)
(environment-define car-environment 'gas-cell #f)
(environment-define car-environment 'rotation-cell #f)
(environment-define car-environment 'internal-car-gps #f)
(environment-define car-environment 'steering-wheel #f)
(environment-define car-environment 'gas #f)

