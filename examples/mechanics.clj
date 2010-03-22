;; Integration of Newton's equations of motion

;; by Konrad Hinsen
;; last updated March 22, 2010

;; Copyright (c) Konrad Hinsen, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(clojure.core/use 'nstools.ns)
(ns+ mechanics
  (:clone nstools.generic-math)
  (:from clojure.contrib.generic.math-functions sqrt)
  (:from units dimension?)
  (:require [units.si :as si]))

;
; A numerical integrator that implements the Velocity-Verlet algorithm
; which is popular in Molecular Dynamics:
; http://en.wikipedia.org/wiki/Verlet_integration#Velocity_Verlet
;

(defn- integrate*
  [dv-fn dt [x v :as state] initial-dv]
  (lazy-seq
   (let [mid-v      (+ v initial-dv)
	 next-x     (+ x (* dt mid-v))
	 next-dv    (dv-fn next-x)
	 next-v     (+ mid-v next-dv)
	 next-state [next-x next-v]]
     (cons next-state
	   (integrate* dv-fn dt next-state next-dv)))))

(defn integrate
  [[force-fn m :as system] [x v :as state] dt]
  {:pre [(si/mass? m) (si/length? x) (si/velocity? v) (si/time? dt)]}
  (let [dt-2-m (/ dt 2 m)
	dv-fn  (comp #(* dt-2-m %) force-fn)]
    (cons state (integrate* dv-fn dt state (dv-fn x)))))

;
; Simple example: the harmonic oscillator
; Compare numerical integration with the known analytic solution
;

(defn oscillator-force-fn
  [k]
  {:pre [(dimension? (/ si/force si/length) k)]}
  (fn [x]
    {:pre [(si/length? x)]}
    (- (* k x))))

(defn analytic-positions
  [k m x0 v0]
  {:pre [(dimension? (/ si/force si/length) k)
	 (si/mass? m)
	 (si/length? x0)
	 (si/velocity? v0)]}
  (let [ang-freq (sqrt (/ k m))
	ampl     (sqrt (+ (sqr x0) (sqr (/ v0 ang-freq))))
	phi      (asin (/ x0 ampl))]
    (fn [t] (* ampl (sin (+ phi (* ang-freq t)))))))

(let [; Define the system parameters
      k        (* 0.5 (/ si/N si/cm))
      m        (si/g 50.)
      ; Define the initial state
      x0       (* 1 si/cm)
      v0       (* -0.2 si/cm (/ si/s))
      ; Choose a suitable time step
      ang-freq (sqrt (/ k m))
      freq     (/ ang-freq (* 2 Math/PI))
      dt       (/ 1 20 freq)]

  (do

    ; Time labels for the trajectories
    (def time-labels (iterate #(+ % dt) (si/s 0.)))

    ; State trajectory (positions and velocities)
    (def trajectory
	 (let [initial-state [x0 v0]
	       system [(oscillator-force-fn k) m]]
       (integrate system initial-state dt)))

    ; Numerical solution for positions
    (def positions
	 (map first trajectory))

    ; Analytic solution for comparison
    (def reference-positions
	 (map (analytic-positions k m x0 v0)
	      time-labels))))

;
; Plot results using Incanter
; 
; This section is commented out to make it possible to run the
; calculation without Incanter. Uncomment if you do have Incanter.
;
(comment

(require 'incanter.core)
(require 'incanter.charts)

; Parameters for the plot
(def N 40) ; show two periods
(def time-unit si/s) ; plot time in seconds
(def length-unit si/cm) ; plot position in centimeters

(doto (incanter.charts/scatter-plot (map #(/ % time-unit)
					 (take N time-labels))
				    (map #(/ % length-unit)
					 (take N positions))
				    :x-label "time [s]"
				    :y-label "position [cm]")
      (incanter.charts/add-lines (map #(/ % time-unit)
				      (take N time-labels))
				 (map #(/ % length-unit)
				      (take N reference-positions)))
      incanter.core/view)

)