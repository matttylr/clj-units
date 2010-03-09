;; Units and physical quantities

;; by Konrad Hinsen
;; last updated March 9, 2010

;; Copyright (c) Konrad Hinsen, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(clojure.core/use 'nstools.ns)
(ns+ units
  "This library defines three types to represent the basic concepts
   of dimensional analysis and unit conversion:

   1) Dimensions (length, time, velocity, ...)
   2) Units  (m, s, ...)
   3) Quantities (3 meters, 10 meters per second, ..)

   A quantity consists of a magnitude and a unit. The magnitude can be of any
   type that implements generic arithmetic (clojure.contrib.generic).
   A unit consists of a factor (a number) and a dimension.
   A unit system defines a set of base dimensions and associated base units.
   Other dimensions are then defined as products of integer powers of the
   base dimensions. Other units are defined as products of integer powers
   of the base dimensions plus a numerical prefactor.

   The generic arithmeric of clojure.contrib.generic.arithmetic is
   implemented for quantities. Units can also be supplied as parameters,
   but the result is always a quantity. Units and quantities can be
   mutiplied with numbers, yielding quantities. Multiplication and
   division are also implemented for dimensions, yielding dimensions.

   The expected use of this library is to define a unit system with its
   base dimensions and units using the defunitsystem macro and then adding
   additional dimensions and units using the defdimension and defunit macros.
   The only functions from this module meant for use in client code are
   in-units-of, and dimension?. Other functions are public because
   they are used in macro expansions, but should not be considered part of
   the stable API of this library."
  (:require [clojure.contrib.generic.arithmetic :as ga]
	    [clojure.contrib.generic.comparison :as gc]
	    [clojure.contrib.string :as string])
  (:from clojure.contrib.generic root-type))


;
; Protocols
;

(defprotocol Quantity
  "Accessing information in quantities and units"
  (dimension [x]
  "return the dimension of quantity or unit x")
  (magnitude [x]
  "return the magnitude of quantity or unit x")
  (magnitude-in-base-units [x]
  "return the magnitude of quantity or unit x relative to the base
   units of the unit system")
  (unit [x]
  "return the unit of quantity or unit x"))

;
; Types
;

(deftype dimension*
  [unit-system
   exponents
   name]
  :as this
  Object
    (equals [#^ ::dimension* o]
      (or (identical? this o)
	  (and (identical? (type o) ::dimension*)
	       (identical? unit-system (:unit-system o))
	       (= exponents (:exponents o)))))
    (hashCode []
      (+ (.hashCode exponents))))

(declare quantity)

(deftype unit*
  [#^Number factor
   #^::dimension* dim
   #^clojure.lang.Symbol name
   #^clojure.lang.Symbol symbol]
  :as this
  clojure.lang.IFn
    (invoke [x] (quantity x this))
  Quantity
    (dimension [] dim)
    (magnitude [] 1)
    (magnitude-in-base-units [] factor)
    (unit [] this)
  Object
    (equals [#^::unit* o]
      (or (identical? this o)
	  (and (identical? (type o) ::unit*)
	       (= dim (dimension o))
	       (= factor (:factor o)))))
    (hashCode []
      (+ (* 31 (.hashCode factor)) (.hashCode dim))))

(deftype quantity
  [m
   #^::unit* u]
  :as this
  Quantity
    (dimension [] (dimension u))
    (magnitude [] m)
    (magnitude-in-base-units [] (ga/* m (magnitude-in-base-units u)))
    (unit [] u)
  Object
    (equals [#^::quantity o]
      (or (identical? this o)
	  (and (identical? (type o) ::quantity)
	       (= (dimension this) (dimension o))
	       (gc/= (magnitude-in-base-units this)
		     (magnitude-in-base-units o)))))
    (hashCode []
      (+ (* 31 (.hashCode m)) (.hashCode u))))

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

(defn make-dimension
  "Create a new dimension. Used by the defdimension macro."
  ([unit-system exponents]
   (make-dimension unit-system exponents nil))
  ([unit-system exponents name]
   (let [dim (dimension* unit-system exponents name)]
     (dosync
       (alter unit-system assoc exponents dim)
       (when name
	 (alter unit-system assoc name dim)))
     dim)))

(defn- get-dimension
  "Return the dimension corresponding to the supplied lists of exponents
   in unit-system, creating it if necessary."
  [unit-system exponents]
  (if-let [dim (@unit-system exponents)]
    dim
    (make-dimension unit-system exponents)))

(defn make-unit
  "Create a new unit. Used by the defunit macro."
  ([factor dimension]
   (make-unit factor dimension nil nil))
  ([factor dimension name symbol]
   (let [unit-system (:unit-system dimension)
	 u           (unit* factor dimension name symbol)]
     (dosync
       (alter unit-system assoc-in [:units dimension factor] u)
       (when name
	 (alter unit-system assoc name u)))
     u)))

(defn- get-unit
  "Return the unit corresponding to the supplied prefactor (relative
   to the base units) and dimension, creating it if necessary."
  [factor dimension]
  (let [unit-system (:unit-system dimension)
	unit        (get-in @unit-system [:units dimension factor])]
    (if unit
      unit
      (make-unit factor dimension))))

(defn as-unit
  "Create a unit from a physical object whose magnitude is a number.
   Used by the defunit macro."
  [quantity unit-name unit-symbol]
  (let [dim    (dimension quantity)
	factor (magnitude-in-base-units quantity)]
    (when-not (number? factor)
      (throw (Exception. "unit prefactor must be a number")))
    (make-unit factor dim unit-name unit-symbol)))

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
  (with-exponents (:base-unit-names @(:unit-system d)) (:exponents d)))

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
	base? (contains? (set (:base-unit-names us)) (:name u))]
    (.write w "#:unit")
    (.write w "{")
    (print-method (:name @(:unit-system d)) w)
    (.write w ":")
    (when (:name d)
      (print-method (:name d) w)
      (.write w ":"))
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
  [#^::quantity x #^java.io.Writer w]
  (let [u (unit x)
	d (dimension x)]
    (.write w "#:")
    (.write w (if (:name d) (str (:name d)) "quantity"))
    (.write w "{")
    (print-method (magnitude x) w)
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
  "Return the quantity expressed in the given unit, which
   must have the same dimension as the quantity."
  [new-unit quantity]
  (let [old-unit (unit quantity)]
    (assert-same-dimension old-unit new-unit)
    (let [factor (/ (:factor old-unit) (:factor new-unit))]
      (new-unit (ga/* factor (magnitude quantity))))))

(defn dimension?
  "Return true if dim is the dimension of quantity."
  [dim quantity]
  (and (contains? #{::quantity ::unit*} (type quantity))
       (= dim (dimension quantity))))

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
; Generic arithmethic and comparison for units, and quantities
;

(derive ::quantity root-type)
(derive ::unit* ::quantity)

(defmethod ga/* [::quantity ::quantity]
  [x y]
  (let [ux  (unit x)
	uy  (unit y)
	dim (ga/* (dimension ux) (dimension uy))
	ctor (if (= (:name dim) 'dimensionless)
	       #(* (:factor ux) (:factor uy) %)
	       (get-unit (* (:factor ux) (:factor uy)) dim))]
  (ctor (ga/* (magnitude x) (magnitude y)))))

(defmethod ga/* [root-type ::quantity]
  [x y]
  ((unit y) (ga/* x (magnitude y))))

(defmethod ga/* [::quantity root-type]
  [x y]
  (ga/* y x))

(ga/defmethod* ga / ::quantity
  [x]
  (let [u (unit x)
	uinv (get-unit (/ (:factor u)) ((ga/qsym ga /) (dimension u)))]
    (uinv ((ga/qsym ga /) (magnitude x)))))

(defmethod ga/+ [::quantity ::quantity]
  [x y]
  (let [u (unit x)
	y (in-units-of u y)]
    (u (ga/+ (magnitude x) (magnitude y)))))

(defmethod ga/- ::quantity
  [x]
  ((unit x) (ga/- (magnitude x))))

(defmethod gc/zero? ::quantity
  [x]
  (gc/zero? (magnitude x)))

(defmethod gc/pos? ::quantity
  [x]
  (gc/pos? (magnitude x)))

(defmethod gc/neg? ::quantity
  [x]
  (gc/neg? (magnitude x)))

(defmethod gc/> [::quantity ::quantity]
  [x y]
  (gc/pos? (magnitude (ga/- x y))))

(defmethod gc/< [::quantity ::quantity]
  [x y]
  (gc/neg? (magnitude (ga/- x y))))

(defmethod gc/= [::quantity ::quantity]
  [x y]
  (gc/zero? (magnitude (ga/- x y))))

;
; Macros for defining unit systems, dimensions, and units
;

(defmacro defdimension*
  "Define dimension name in terms of its exponents relative to the
   base dimensions of unit-system."
  [unit-system name exponents]
  (let [type-kw    (keyword (str (ns-name *ns*)) (str name))
	query-name (symbol (str name "?"))]
    `(let [exp# ~exponents]
       (def ~(symbol (str *ns*) (str name))
         (make-dimension ~unit-system exp# ~(list 'quote name)))
       (def ~(symbol (str *ns*) (str query-name))
         (partial dimension? ~name)))))

(defmacro defunit
  ([unit-symbol unit-name factor dimension]
   `(def ~(symbol (str *ns*) (str unit-symbol))
      (make-unit ~factor ~dimension
		 (quote ~(symbol unit-name)) (quote ~unit-symbol))))
  ([unit-symbol unit-name quantity]
   `(def ~(symbol (str *ns*) (str unit-symbol))
      (as-unit ~quantity (quote ~(symbol unit-name)) (quote ~unit-symbol)))))

(defmacro defdimension
  "Define dimension name in unit-system "
  ([name dims-and-expts]
   (let [dims-and-expts (partition 2 dims-and-expts)
	 unit-systems   (map (comp :unit-system first) dims-and-expts)
	 first-dim      (first (first dims-and-expts))
	 dims-and-expts (map #(cons 'list %) dims-and-expts)]
     (assert (apply = unit-systems))
     `(defdimension* (:unit-system ~first-dim) ~name
	(reduce (fn [a# b#] (map + a# b#))
		(map (fn [[d# e#]]
		       (map (partial * e#) (:exponents d#)))
		     ~(cons 'list dims-and-expts))))))
  ([name unit-name unit-symbol dims-and-expts]
   `(do (defdimension ~name ~dims-and-expts)
	(defunit ~unit-symbol ~unit-name 1 ~name))))

(defmacro defunitsystem
  "Define a unit system in terms of base dimensions and associated
   base units. Each dimension is specified by a dimension name
   (a symbol), a unit name (a string) and a unit symbol (a symbol).
   The dimension and unit symbols will be def'd in the current
   namespace. Additionally, for each dimension a dimension
   predicate will be created and def'd to a symbol made by adding
   a question mark to the dimension name."
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
	unit-defs      (map (fn [[d u s]] `(defunit ~s ~u 1 ~d))
			    entries)]
    `(do
       (def ~us-name (ref {:name (quote ~us-name)
			   :base-dimensions ~(quote-all dimensions)
			   :base-unit-names ~(quote-all units)
			   :base-unit-symbols ~(quote-all unit-symbols)
			   :units {}}))
       (let [exp# ~(cons 'list (repeat (count dimensions) 0))]
	 (make-dimension ~us-name exp# (quote ~'dimensionless)))
       ~@dimension-defs
       ~@unit-defs)))

(defmacro defprefixedunits
  [unit-system exclusions & prefixes]
  (let [units     (apply concat
			 (map vals (vals (@@(resolve unit-system) :units))))
	units     (filter (complement #(contains? (set exclusions) %)) units)
	unit-defs (for [u units
			[s p f] (partition 3 prefixes)]
		    `(defunit ~(symbol (str s (:symbol u)))
		       ~(str p (:name u))
		       (ga/* ~f ~(symbol (str *ns*) (str (:symbol u))))))]
    `(do ~@unit-defs)))
