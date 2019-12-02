(ns advent2019.core-test
  (:require [cljs.test :refer (deftest is)]
            [advent2019.core :as core]))


(deftest module-fuel-requirement-test
  (is (= 2 (core/module-fuel-requirement 12)))
  (is (= 2 (core/module-fuel-requirement 14)))
  (is (= 966 (core/module-fuel-requirement 1969)))
  (is (= 50346 (core/module-fuel-requirement 100756))))
