;; SI unit system

;; by Konrad Hinsen
;; last updated March 8, 2010

;; Copyright (c) Konrad Hinsen, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(clojure.core/use 'nstools.ns)
(ns+ units.si
  (:clone nstools.generic-math)
  (:remove force time)
  (:from units defunitsystem defdimension defunit))

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
(defdimension SI area
  [length 2])
(defdimension SI volume
  [length 3])
(defdimension SI frequency "Hertz" Hz
  [time -1])
(defdimension SI velocity
  [length 1 time -1])
(defdimension SI acceleration
  [velocity 1 time -1])
(defdimension SI force "Newton" N
  [mass 1 acceleration 1])
(defdimension SI energy "Joule" J
  [mass 1 velocity 2])
(defdimension SI pressure "Pascal" Pa
  [force 1 area -1])

;
; Electrical dimensions and units
;
(defdimension SI electric-charge "Coulomb" C
  [electric-current 1 time 1])


(defunit km "kilometer" (* 1000 m))

(defunit mn "minute" (* 60 s))
(defunit h "hour" (* 60 mn))
