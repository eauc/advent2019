(ns advent2019.core
  (:require ["fs" :as fs]))


(defn main [& cli-args]
  (prn "hello world"))


(defn module-fuel-requirement
  [module-mass]
  (let [fuel-mass
        (max 0
             (- (js/Math.floor
                  (/ module-mass 3)) 2))]
    (if (>= 0 fuel-mass)
      fuel-mass
      (+ fuel-mass (module-fuel-requirement fuel-mass)))))


(comment

  (reduce + (map #(-> % (js/parseInt 10) module-fuel-requirement) input-lines))
  ;; 5115845
  ;; 3412496

  (def input-lines
    (clojure.string/split-lines
      (fs/readFileSync "./input.txt"))))
