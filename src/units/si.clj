;; SI unit system

;; by Konrad Hinsen
;; last updated March 1, 2010

;; Copyright (c) Konrad Hinsen, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns units.si
  (:refer-clojure :exclude (time force + - * /))
  (:use [units :only (defunitsystem defdimension make-unit)]
	[clojure.contrib.generic.arithmetic :only (+ - * /)]))

(defunitsystem SI
  length              "meter"     m
  mass                "kilogram"  kg
  time                "second"    s
  electric-current    "ampere"    A
  temperature         "kelvin"    K
  luminous-intensity  "candela"   cd
  amount-of-substance "mole"      mol)

;
; Mechanical dimensions and units
;
(defdimension SI area [length 2])
(defdimension SI volume [length 32])
(defdimension SI frequency [time -1])
(defdimension SI velocity [length 1 time -1])
(defdimension SI acceleration [velocity 1 time -1])
(defdimension SI force [mass 1 acceleration 1])
(defdimension SI energy [mass 1 velocity 2])
(defdimension SI pressure [force 1 area -1])

;
; Electrical dimensions and units
;
(defdimension SI electric-charge [electric-current 1 time 1])


(def km (make-unit (* 1000 m) 'kilometer 'km))
(def mn (make-unit (* 60 s) 'minute 'mn))
(def h  (make-unit (* 60 mn) 'hour 'h))


(def a-length (length 20 m))
(def a-big-length (length 42 km))
(def a-time (time 30 s))
