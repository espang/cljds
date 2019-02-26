(ns cljds.test.avl
  (:require [clojure.test :refer :all]
            [cljds.avl :refer :all]
            [cljds.core :as c]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def log_e_2 (Math/log 2))

(defn log2 [v]
  (/ (Math/log v) log_e_2))

(defn minimum-height [n]
  (Math/floor (log2 n)))

(defn maximum-height [n]
  (Math/ceil (log2 (inc n))))

; test that the height is limited which should be the case
; if balancing works. limit to unique values
(defspec height-is-limited
  100
  (prop/for-all [v (gen/vector gen/int)]
    (let [t           (seq->avl v)
          entries     (count (distinct v))
          lower-bound (minimum-height entries)
          upper-bound (maximum-height entries)
          h           (height t)]
      (<= lower-bound h upper-bound))))

(defspec contains-all-elements
  100
  (prop/for-all [v (gen/vector gen/int)]
    (let [t (seq->avl v)]
      (every? (partial contains-element? t) v))))

(deftest test-height-failure
  (testing "height-is-limited failures"
    ; [4 3 2 5 6 1]
    ; https://www.cs.usfca.edu/~galles/visualization/AVLtree.html
    ; [3 [2 1 nil] [5 4 6]]
    (let [tree (seq->avl [4 3 2 5 6 1])]
      (is (= tree
             (c/->Node 3 (c/->Node 2 1 nil)
                         (c/->Node 5 4 6)))))))
