;; Use of units in accounting applications

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
(ns+ accounting
  (:clone nstools.generic-math)
  (:remove time)
  (:from units defunitsystem defdimension defunit in-units-of)
  (:require [clojure.xml :as xml]))

;
; Note: I know almost nothing about accounting. The point of this example
; is to illustrate how units can be of interest outside of science and
; engineering. Perhaps it is very far from what is needed for really
; doing accounting.
;

(defunitsystem accounting
  money     "euro"   EUR
  time      "hour"   h
  workforce "person" person)

(defdimension salary [money 1 time -1 person -1])

; workday = 8 hours
(defunit d "day" (* 8 h))

; week = 5 workdays
(defunit w "week" (* 5 d))

; month = 4 weeks
(defunit m "month" (* 4 w))

; year = 52 weeks
(defunit y "year" (* 52 w))


; Define currencies using the daily updated exchange rates of the
; European Central Bank
;
; Note: the exchange rates are read when the code is run, but are never
; updated later on. Don't use this for long-running scripts that require
; up-to-date information!

(defn- read-exchange-rates
  []
  (map (comp (fn [m] [(:currency m) (Float/valueOf (:rate m))]) xml/attrs)
       (xml/content
	(first
	 (xml/content
	  (first
	   (filter #(= (xml/tag %) :Cube)
		   (xml/content
		    (xml/parse "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")))))))))

(defmacro defcurrencies
  []
  (cons 'do
	(for [[name rate] (read-exchange-rates)]
	  `(defunit ~(symbol name) ~name (/ EUR ~rate)))))

(defcurrencies)


;
; Some application examples
;

; The French legal minimum salary for workers paid by the hour
(def smic-horaire (/ (* 8.86 EUR) h person))

; Print the minimum salary for five workers in US dollars per year
(prn (/ (* smic-horaire (person 5)) (/ USD y)) "dollars per year")
