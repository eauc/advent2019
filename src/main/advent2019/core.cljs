(ns advent2019.core
  (:require ["fs" :as fs]
            [clojure.set]))

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


(defn decoded-wire
  [wire]
  (map #(vector (subs % 0 1)
                (js/parseInt (subs % 1) 10)) wire))


(defmulti move->positions
  (fn [[direction distance] from]
    direction))

(defmethod move->positions "R"
  [[direction distance] [from-x from-y]]
  (map
    #(vector (+ from-x (inc %)) from-y)
    (range distance)))

(defmethod move->positions "L"
  [[direction distance] [from-x from-y]]
  (map
    #(vector (- from-x (inc %)) from-y)
    (range distance)))

(defmethod move->positions "D"
  [[direction distance] [from-x from-y]]
  (map
    #(vector from-x (- from-y (inc %)))
    (range distance)))

(defmethod move->positions "U"
  [[direction distance] [from-x from-y]]
  (map
    #(vector from-x (+ from-y (inc %)))
    (range distance)))


(defn wire->positions
  [wire]
  (loop [[move & moves-rest] (decoded-wire wire)
         positions [[0 0]]]
    (if (nil? move)
      (rest positions)
      (let [new-positions (move->positions move (last positions))]
        (recur moves-rest (concat positions new-positions))))))


(defn wires-intersections
  [wire-positions]
  (apply
    clojure.set/intersection
    (map set wire-positions)))

(defn distance-to-point
  [wire-positions point]
  (inc (count (take-while #(not= point %) wire-positions))))


(defn wires->closest-intersection
  [wires]
  (let [wires-positions (map wire->positions wires)
        intersections (wires-intersections wires-positions)
        int-distances (map
                        (fn [positions]
                          (map #(distance-to-point positions %) intersections))
                        wires-positions)]
    (apply min
           (apply map #(+ (js/Math.abs %1)
                          (js/Math.abs %2))
                  int-distances))))


(comment

  (move->positions ["R" 8] [10 5])
  (move->positions ["D" 8] [10 5])
  (move->positions ["L" 8] [10 5])
  (move->positions ["U" 8] [10 5])

  (wire->positions ["R5" "D3" "L1" "U2"])
  (wire->positions ["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"])
  (wire->positions ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"])
  (wires->closest-intersection
    [["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"]
     ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"]])
  (wires->closest-intersection
    [["R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"]
     ["U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7"]])

  (def wires
    (map
      #(-> %
           (clojure.string/split ","))
      (clojure.string/split-lines
        (fs/readFileSync "./wires.txt"))))
  (wires->closest-intersection
    wires)

  )


(defn main [& cli-args]
  (prn "hello world")
  (prn
    (wires->closest-intersection
      (map
        #(-> %
             (clojure.string/split ","))
        (clojure.string/split-lines
          (fs/readFileSync "./wires.txt"))))))
