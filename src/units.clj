;; Units and physical quantities

;; by Konrad Hinsen
;; last updated March 2, 2010

;; Copyright (c) Konrad Hinsen, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns units
  (:refer-clojure :exclude (time force))
  (:require [clojure.contrib.generic.arithmetic :as ga]
	    [clojure.contrib.string :as string])
  (:use [clojure.contrib.generic :only (root-type)]))

;
; Protocols
;

(defprotocol Quantity
  (dimension [x])
  (magnitude [x])
  (unit [x]))

;
; Types
;

(deftype dimension*
  [unit-system
   exponents
   quantity-ctor
   name])

(deftype unit*
  [#^Number factor
   #^::dimension* dimension
   #^clojure.lang.Symbol name
   #^clojure.lang.Symbol symbol]
  :as this
  clojure.lang.IPersistentMap
  clojure.lang.IFn
    (invoke [x] ((:quantity-ctor dimension) x this))
  Quantity
    (dimension [] dimension)
    (magnitude [] 1)
    (unit [] this))

(deftype quantity
  [magnitude unit]
  :as this
  Quantity
  (dimension [] (dimension unit))
  (magnitude [] magnitude)
  (unit [] unit))

(remove-method print-method ::quantity)

;
; Error checking
;

(defn- assert-same-unit-system
  [d1 d2]
  (let [us1 (:unit-system d1)
	us2 (:unit-system d2)]
    (when-not (identical? us1 us2)
      (throw (Exception. (str "Can't combine units from "
			      (@us1 :name) " and " (@us2 :name)))))))

(defn- assert-same-dimension
  [u1 u2]
  (let [d1 (dimension u1)
	d2 (dimension u2)]
    (assert-same-unit-system d1 d2)
    (when-not (identical? d1 d2)
      (throw (Exception. (str "cannot convert " d1 " to " d2))))))

;
; Retrieving and constructing dimensions and units
;

(defn- make-anonymous-dimension
  [unit-system exponents]
  (let [dim (dimension* unit-system exponents quantity nil)]
    (swap! unit-system assoc exponents dim)
    dim))

(defn- get-dimension
  [unit-system exponents]
  (if-let [dim (@unit-system exponents)]
    dim
    (make-anonymous-dimension unit-system exponents)))

(defn make-unit
  [quantity name symbol]
  (when-not (number? (magnitude quantity))
    (throw (Exception. "Unit prefactor must be a number")))
  (unit* (* (magnitude quantity) (:factor (unit quantity)))
	 (dimension quantity)
	 name
	 symbol))

;
; String formatting and printing
;

(defn- with-exponents
  [names exponents]
  (string/join "." (filter (comp pos? count)
			   (map (fn [n e] (case e  0 ""  1 (str n)  (str n e)))
				names exponents))))

(defn- base-dimensions-with-exponents
  [d]
  (with-exponents (:base-dimensions @(:unit-system d)) (:exponents d)))

(defn- base-units-with-exponents
  [d]
  (with-exponents (:base-units @(:unit-system d)) (:exponents d)))

(defn- base-unit-symbols-with-exponents
  [d]
  (with-exponents (:base-unit-symbols @(:unit-system d)) (:exponents d)))

(defn- dimension-name
  [d]
  (if-let [n (:name d)]
    n
    (base-dimensions-with-exponents d)))

(defn- unit-name
  [u]
  (if-let [n (:name u)]
    n
    (base-units-with-exponents (dimension u))))

(defn- unit-symbol
  [u]
  (if-let [n (:symbol u)]
    n
    (base-unit-symbols-with-exponents (dimension u))))

(defmethod print-method ::dimension*
  [d #^java.io.Writer w]
  (let [us @(:unit-system d)
	base? (contains? (set (:base-dimensions us)) (:name d))]
    (.write w "#:dimension")
    (.write w "{")
    (print-method (:name us) w)
    (.write w ":")
    (when (:name d)
      (print-method (:name d) w)
      (when-not base?
	(.write w "=")))
    (when-not base?
      (.write w (base-dimensions-with-exponents d)))
    (.write w "}")))

(defmethod print-method ::unit*
  [u #^java.io.Writer w]
  (let [d (dimension u)
	us @(:unit-system d)
	base? (contains? (set (:base-units us)) (:name u))]
    (.write w "#:unit")
    (.write w "{")
    (print-method (:name @(:unit-system (dimension u))) w)
    (.write w ":")
    (when (:name u)
      (print-method (:name u) w)
      (when (:symbol u)
	(.write w "(")
	(print-method (:symbol u) w)
	(.write w ")"))
      (when-not base?
	(.write w "=")))
    (when-not base?
      (print-method (:factor u) w)
      (.write w ".")
      (.write w (base-unit-symbols-with-exponents (dimension u))))
    (.write w "}")))

(defmethod print-method ::quantity
  [x #^java.io.Writer w]
  (let [u (unit x)
	d (dimension x)]
    (.write w "#:")
    (.write w (if (:name d) (str (:name d)) "quantity"))
    (.write w "{")
    (print-method (:magnitude x) w)
    (.write w " ")
    (cond
     (:symbol u)  (.write w (str (:symbol u)))
     (:name u)    (.write w (str (:name u)))
     :else        (do (when-not (= (:factor u) 1)
			(print-method (:factor u) w)
			(.write w "."))
		      (.write w (base-unit-symbols-with-exponents d))))
    (.write w "}")))

;
; Functions for use with units and quantities
;

(defn in-units-of
  [new-unit quantity]
  (let [old-unit (unit quantity)]
    (assert-same-dimension old-unit new-unit)
    (let [factor (/ (:factor old-unit) (:factor new-unit))]
      (new-unit (ga/* factor (:magnitude quantity))))))

;
; Generic arithmethic for dimensions
;

(derive ::dimension* root-type)

(defmethod ga/* [::dimension* ::dimension*]
  [d1 d2]
  (assert-same-unit-system d1 d2)
  (get-dimension (:unit-system d1) (map + (:exponents d1) (:exponents d2))))

(ga/defmethod* ga / ::dimension*
  [d]
  (get-dimension (:unit-system d) (map - (:exponents d))))

;
; Generic arithmethic for units, and quantities
;

(derive ::quantity root-type)
(derive ::unit* ::quantity)

(defmethod ga/* [::quantity ::quantity]
  [x y]
  (let [ux (unit x)
	uy (unit y)
	uprod (unit* (* (:factor ux) (:factor uy))
		     (ga/* (:dimension ux) (:dimension uy)) nil nil)]
  (uprod (ga/* (magnitude x) (magnitude y)))))

(defmethod ga/* [root-type ::quantity]
  [x y]
  ((unit y) (ga/* x (magnitude y))))

(defmethod ga/* [::quantity root-type]
  [x y]
  (ga/* y x))

(ga/defmethod* ga / ::quantity
  [x]
  (let [u (unit x)
	uinv (unit* (/ (:factor u)) ((ga/qsym ga /) (:dimension u)) nil nil)]
    (uinv ((ga/qsym ga /) (magnitude x)))))

(defmethod ga/+ [::quantity ::quantity]
  [x y]
  (let [u (unit x)
	y (in-units-of u y)]
    (u (ga/+ (magnitude x) (magnitude y)))))

(defmethod ga/- ::quantity
  [x]
  ((unit x) (ga/- (magnitude x))))

;
; Macros for defining unit systems, dimensions, and units
;
(defmacro defdimension*
  [unit-system name exponents]
  (let [type-kw (keyword (str (ns-name *ns*)) (str name))]
    `(do (deftype ~name [~'magnitude ~'unit] :as ~'this
	   Quantity
	   (~'dimension [] (dimension ~'unit))
	   (~'magnitude [] ~'magnitude)
	   (~'unit [] ~'unit))
	 (remove-method print-method ~type-kw)
	 (derive ~type-kw ::quantity)
	 (let [exp# ~exponents
	       dim# (dimension* ~unit-system exp# ~name ~(list 'quote name))]
	   (swap! ~unit-system assoc exp# dim#)
	   (swap! ~unit-system assoc ~(list 'quote name) dim#)))))

(defmacro defdimension
  ([unit-system name dims-and-expts]
   (let [dims-and-expts (partition 2 dims-and-expts)]
     `(defdimension* ~unit-system ~name
	(reduce (fn [a# b#] (map + a# b#))
		(map (fn [[d# e#]]
		       (map (partial * e#)
			    (get-in @~unit-system [d# :exponents])))
		     ~(list 'quote dims-and-expts))))))
  ([unit-system name unit unit-symbol dims-and-expts]
   `(do (defdimension ~unit-system ~name ~dims-and-expts)
	(def ~unit-symbol
	  (unit* 1 ((deref ~unit-system) (quote ~name))
		 (quote ~(symbol unit)) (quote ~unit-symbol))))))

(defmacro defunitsystem
  [us-name & entries]
  (let [quote-all      (fn [v] (vec (map #(list 'quote %) v)))
	entries        (partition 3 entries)
	dimensions     (vec (map first entries))
	units          (vec (map (comp symbol second) entries))
	unit-symbols   (vec (map #(nth % 2) entries))
	exponents      (fn [d] (map (fn [x] (if (= x d) 1 0)) dimensions))
	dimension-defs (map (fn [d] `(defdimension* ~us-name ~d
				       ~(list 'quote (exponents d))))
			    dimensions)
	unit-defs      (map (fn [[d u s]]
			      `(def ~s (unit* 1 ((deref ~us-name) (quote ~d))
					      (quote ~(symbol u))
					      (quote ~s))))
			    entries)]
    `(do
       (def ~us-name (atom {:name (quote ~us-name)
			    :base-dimensions ~(quote-all dimensions)
			    :base-units ~(quote-all units)
			    :base-unit-symbols ~(quote-all unit-symbols)}))
       (let [exp# ~(cons 'list (repeat (count dimensions) 0))
	     dim# (dimension* ~us-name exp# (fn [a# b#] a#) nil)]
	 (swap! ~us-name assoc exp# dim#))
       ~@dimension-defs
       ~@unit-defs)))
