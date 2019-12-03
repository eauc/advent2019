(ns advent2019.core-test
  (:require [cljs.test :refer (deftest is)]
            [advent2019.core :as core]))


(deftest module-fuel-requirement-test
  (is (= 2 (core/module-fuel-requirement 12)))
  (is (= 2 (core/module-fuel-requirement 14)))
  (is (= 966 (core/module-fuel-requirement 1969)))
  (is (= 50346 (core/module-fuel-requirement 100756))))

(deftest run-program-test
  (is (= [99] (core/run-program [99])))
  (is (= [2 0 0 0 99] (core/run-program [1 0 0 0 99])))
  (is (= [2 3 0 6 99] (core/run-program [2 3 0 3 99])))
  (is (= [2 4 4 5 99 9801] (core/run-program [2 4 4 5 99 0])))
  (is (= [30 1 1 4 2 5 6 0 99] (core/run-program [1 1 1 4 99 5 6 0 99]))))
