;; Tests for SI units

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
(ns+ si-tests
  (:clone units.si)
  (:from units dimension?)
  (:from clojure.test deftest is are run-tests))

(deftest dimension-arithmetic
  (are [x y] (= x y)
       (* length length)     area
       (/ area length)       length
       (* velocity time)     length
       (* mass acceleration) force
       (/ force area)        pressure)
  (are [x] (thrown? Exception x)
       (+ length length)
       (- time area)))

(deftest dimensions-of
  (are [x y] (dimension? y x)
       m        length
       kg       mass
       (/ m s)  velocity
       (/ C s)  electric-current
       (* N m)  energy))

(deftest quantity-arithmetic
  (are [x y] (= x y)
       (+ (m 20) (m 30))  (m 50)
       (+ (km 5) (m 25))  (m 5025)
       (* (N 3) (m 2/3))  (J 2)
       (* 2 N m)          (* 2 J)
       (+ (Hz 1) (/ 1 s)) (Hz 2)
       (+ (Bq 1) (/ 1 s)) (Bq 2))
  (are [x] (thrown? Exception x)
       (+ (m -3) (s 5))
       (- (kg 4) (J 3))
       (+ (Bq 2) (Hz 3))))

(deftest quantity-comparison
  (are [x] x
       (> (m 20) (m 10))
       (>= (m 20) (m 10))
       (< (s 5) (h 1))
       (<= (kg 2) (kg 2))
       (pos? (s 3))
       (neg? (s -3))
       (zero? (s 0)))
  (are [x] (thrown? Exception x)
       (< (m -3) (s 5))
       (> (kg 4) (J 3))))
  

(run-tests)
