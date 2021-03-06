;;; ----------------------------------------------------------------------
;;; Copyright 2016 Alexey Radul and Gerald Jay Sussman
;;; ----------------------------------------------------------------------
;;; This file is part of New Artistic Propagator Prototype.  It is
;;; derived from the Artistic Propagator Prototype previously
;;; developed by Alexey Radul and Gerald Jay Sussman.
;;; 
;;; New Artistic Propagator Prototype is free software; you can
;;; redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software
;;; Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;; 
;;; New Artistic Propagator Prototype is distributed in the hope that
;;; it will be useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with New Artistic Propagator Prototype.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

;;; To load the full system, either load this file with
;;; (load "path/to/load-published.scm") or change your Scheme's
;;; working directory to be the directory where this file is and
;;; evaluate these forms sequentially at the repl.

;;; This version is intended to be used with the Scmutils
;;; symbolic/numeric system, developed by Gerald Jay Sussman, Jack
;;; Wisdom, and many other friends over many years.

(if (not (environment-bound? system-global-environment
                             'user-generic-environment))
    (error "Please load me into a scmutils-augmented Scheme"))

(environment-define system-global-environment 'user-propagator-environment
		    (extend-top-level-environment
		     (access user-generic-environment scmutils-base-environment)))

(define (cf-conditionally filename)
  (fluid-let ((sf/default-syntax-table (nearest-repl/environment)))
    (sf-conditionally filename))
  (if (not (file-processed? filename "bin" "com"))
      (compile-bin-file filename)))

(define (load-compiled filename environment)
  (cf-conditionally filename)
  (load filename environment))

(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative-compiled filename)
  (self-relatively
   (lambda ()
     (load-compiled filename user-propagator-environment))))

(load-relative "sugar")

(for-each
 load-relative-compiled
 '("utils"
   ;; "memoizers"     ;unused
   "eq-properties"    ;supplied by scmutils except for eq-delete
   "sugar"
   ;;"ghelper"        ;supplied by scmutils
   "genops"
   "cells"            ;defines merge generic operator

   "intervals"
   "propagators"
   "primitive-propagators"
   "constraint-propagators"
   "supported-values"
   "tms"
   "search"
   "plunk-and-solve"
   "ui"
   "scheduler"

   ;; Test cases
   "puzzle-utilities"
   "new-examples-test"
   "electric"))

(stop-scmutils-print)
((access initialize-scheduler user-propagator-environment))
(ge user-propagator-environment)


