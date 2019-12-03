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

(deftest wires-test
  (is (= 610 (core/wires->closest-intersection
               [["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"]
                ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"]])))
  (is (= 410 (core/wires->closest-intersection
               [["R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"]
                ["U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7"]]))))
