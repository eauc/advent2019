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

  (reduce + (map module-fuel-requirement modules-masses))
  ;; 5115845
  ;; 3412496

  (def modules-masses
    (map
      #(js/parseInt % 10)
      (clojure.string/split-lines
        (fs/readFileSync "./modules_masses.txt"))))

  )


(defn assoc-output
  [output n value]
  (concat (take n output)
          (list value)
          (drop (inc n) output)))


(defmulti run-opcode
  (fn [program output]
    (first program))
  :default 99)


(defmethod run-opcode 99
  [program output]
  [(list) output])

(defmethod run-opcode 1
  [[_ op-a op-b op-result & prog-rest] output]
  (let [result (+ (nth output op-a)
                  (nth output op-b))]
    [prog-rest (assoc-output output op-result result)]))

(defmethod run-opcode 2
  [[_ op-a op-b op-result & prog-rest] output]
  (let [result (* (nth output op-a)
                  (nth output op-b))]
    [prog-rest (assoc-output output op-result result)]))


(defn run-program
  ([program output]
   (if (= 0 (count program))
     output
     (let [[prog-rest new-output] (run-opcode program output)]
       (run-program (take-last (count prog-rest) new-output) new-output))))
  ([program]
   (run-program program program)))


(defn call-program
  [program noun verb]
  (-> program
      (assoc-output 1 noun)
      (assoc-output 2 verb)
      run-program
      first))


(comment

  (run-opcode [99] [1 0])
  (run-opcode [1 0 0 0 99] [1 0 0 0 99])
  (run-opcode [2 3 0 3 99] [2 3 0 3 99])

  (run-program [99])
  (run-program [1 0 0 0 99])
  (run-program [2 3 0 3 99])
  (run-program [2 4 4 5 99 0])
  (run-program [1 1 1 4 99 5 6 0 99])

  (def gravity-program
    (map
      #(js/parseInt % 10)
      (clojure.string/split
        (fs/readFileSync "./gravity_program.txt")
        #",")))

  (call-program gravity-program 12 2)
  ;; 4570637

  (filter
    #(= 19690720 (nth % 2))
    (for [noun (range 30 60)
          verb (range 99)]
      [noun verb (call-program gravity-program noun verb)]))

  )
