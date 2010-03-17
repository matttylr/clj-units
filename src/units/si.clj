;; SI unit system

;; by Konrad Hinsen
;; last updated March 17, 2010

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
  (:remove force time ns min)
  (:from units defunitsystem defdimension defunit defprefixedunits))

(defunitsystem SI
  length              "meter"     m
  mass                "kilogram"  kg
  time                "second"    s
  electric-current    "ampere"    A
  temperature         "kelvin"    K
  luminous-intensity  "candela"   cd
  amount-of-substance "mole"      mol)

;
; Geometrical and mechanical dimensions and units
;
(defdimension area
  [length 2])
(defdimension volume
  [length 3])
(defdimension frequency "hertz" Hz
  [time -1])
(defdimension velocity
  [length 1 time -1])
(defdimension acceleration
  [velocity 1 time -1])
(defdimension force "newton" N
  [mass 1 acceleration 1])
(defdimension energy "joule" J
  [mass 1 velocity 2])
(defdimension power "watt" W
  [energy 1 time -1])
(defdimension pressure "pascal" Pa
  [force 1 area -1])

(defdimension angle "radian" rad
  [length 1 length -1])
(defdimension solid-angle "steradian" sr
  [area 1 area -1])

;
; Electrical dimensions and units
;
(defdimension electric-charge "coulomb" C
  [electric-current 1 time 1])
(defdimension voltage "volt" V
  [energy 1 electric-charge -1])
(defdimension capacitance "farad" F
  [electric-charge 1 voltage -1])
(defdimension resistance "ohm" Î©
  [voltage 1 electric-current -1])
(defdimension conductance "siemens" S
  [resistance -1])
(defdimension magnetic-flux "weber" Wb
  [energy 1 electric-current -1])
(defdimension magnetic-field-strength "tesla" T
  [magnetic-flux 1 area -1])
(defdimension inductance "henry" H
  [magnetic-flux 1 electric-current -1])

;
; Other dimensions and units
;
(defdimension luminous-flux "lumen" lm
  [luminous-intensity 1 solid-angle 1])

(defdimension illuminance "lux" lx
  [luminous-flux 1 area -1])

(defdimension radioactivity "becquerel" Bq
  [time -1])

(defdimension absorbed-dose "gray" Gy
  [energy 1 mass -1])

(defdimension equivalent-dose "sievert" Sv
  [energy 1 mass -1])

(defdimension catalytic-activity "katal" kat
  [amount-of-substance 1 time -1])

;
; Apply standard SI prefixes to all units defined until here
;
(defunit g "gram" (/ kg 1000))

(defprefixedunits SI [kg]
  Y  "yotta" 1000000000000000000000000
  Z  "zetta" 1000000000000000000000
  E  "exa"   1000000000000000000
  P  "peta"  1000000000000000
  T  "tera"  1000000000000
  G  "giga"  1000000000
  M  "mega"  1000000
  k  "kilo"  1000
  h  "hecto" 100
  da "deca"  10
  d  "deci"  1/10
  c  "centi" 1/100
  m  "milli" 1/1000
  μ  "micro" 1/1000000
  n  "nano"  1/1000000000
  p  "pico"  1/1000000000000
  f  "femto" 1/1000000000000000
  a  "ato"   1/1000000000000000000
  z  "zepto" 1/1000000000000000000000
  y  "yocto" 1/1000000000000000000000000)

;
; Units accepted for use with SI
;
(defunit l "liter" (* dm dm dm))
(defunit dl "deciliter" (/ l 10))
(defunit cl "centiliter" (/ l 100))
(defunit ml "milliliter" (/ l 1000))
(defunit μl "microliter" (/ l 1000))

(defunit min "minute" (* 60 s))
(defunit h "hour" (* 60 min))
(defunit d "day" (* 24 h))

(defunit deg "degree" (* (/ Math/PI 180) rad))

(defunit ha "hectare" (* 10000 m m))

(defunit t "tonne" (* 1000 kg))

;
; Non-SI units whose use is discouraged
;
(defunit Å "angstrøm" (* 1/10 nm))

(defunit bar "bar" (* 100000 Pa))
(defunit mbar "millibar" (* 1/1000 bar))
(defunit atm "atmosphere" (* (/ 101325 100) mbar))

(defunit a "are" (* 100 m m))
(defunit b "barn" (* 1/10000 pm pm))

;
; Units based on fundamental constants
;

(defunit eV "electronvolt" (* 1.60217733e-19 C V))
(defunit meV "millielectronvolt" (* 1/1000 eV))
(defunit μeV "microelectronvolt" (* 1/1000 meV))

(defunit amu "atomic-mass-unit" (* 1.6605402e-27 kg))

(defunit AU "astronomical-unit" (* 1.49597870691e11 m))

