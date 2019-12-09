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

(defn parameter
  [pc param-index memory]
  (let [opcode (nth memory pc)
        param-mode (-> opcode
                       (/ (js/Math.pow 10 (+ param-index 2)))
                       js/Math.floor
                       (mod 10)
                       (= 1)
                       (if :immediate :position))
        param-value (nth memory (+ pc 1 param-index))]
    (if (= param-mode :immediate)
      param-value
      (nth memory param-value))))

(comment
  (parameter 2 0 [0 1 2    4 5 6])
  ;; 5
  (parameter 2 0 [0 1 102  4 5 6])
  ;; 4
  (parameter 2 1 [0 1 2    4 5 6])
  ;; 6
  (parameter 2 1 [0 1 1002 4 5 6])
  ;; 5

  )


(defmulti run-opcode
  (fn [memory pc input output]
    (mod (nth memory pc) 100))
  :default 99)


(defmethod run-opcode 99
  [memory pc input output]
  [memory -1 input output])

(defmethod run-opcode 1
  [memory pc input output]
  (let [[_ param-a param-b param-result & prog-rest] (drop pc memory)
        a (parameter pc 0 memory)
        b (parameter pc 1 memory)
        result (+ a b)]
    [(assoc memory param-result result) (+ pc 4) input output]))

(defmethod run-opcode 2
  [memory pc input output]
  (let [[_ param-a param-b param-result & prog-rest] (drop pc memory)
        a (parameter pc 0 memory)
        b (parameter pc 1 memory)
        result (* a b)]
    [(assoc memory param-result result) (+ pc 4) input output]))

(defmethod run-opcode 3
  [memory pc input output]
  (let [[_ param-to & prog-rest] (drop pc memory)]
    [(assoc memory param-to (first input)) (+ pc 2) (subvec input 1) output]))

(defmethod run-opcode 4
  [memory pc input output]
  (let [[_ param-from & prog-rest] (drop pc memory)
        from (parameter pc 0 memory)]
    [memory (+ pc 2) input (conj output from)]))

(defmethod run-opcode 5
  [memory pc input output]
  (let [[_ param-a param-result & prog-rest] (drop pc memory)
        test-value (parameter pc 0 memory)
        jump-to (parameter pc 1 memory)
        new-pc (if (not= test-value 0) jump-to (+ pc 3))]
    [memory new-pc input output]))

(defmethod run-opcode 6
  [memory pc input output]
  (let [[_ param-a param-result & prog-rest] (drop pc memory)
        test-value (parameter pc 0 memory)
        jump-to (parameter pc 1 memory)
        new-pc (if (= test-value 0) jump-to (+ pc 3))]
    [memory new-pc input output]))

(defmethod run-opcode 7
  [memory pc input output]
  (let [[_ param-a param-b param-result & prog-rest] (drop pc memory)
        a (parameter pc 0 memory)
        b (parameter pc 1 memory)
        result (if (< a b) 1 0)]
    [(assoc memory param-result result) (+ pc 4) input output]))

(defmethod run-opcode 8
  [memory pc input output]
  (let [[_ param-a param-b param-result & prog-rest] (drop pc memory)
        a (parameter pc 0 memory)
        b (parameter pc 1 memory)
        result (if (= a b) 1 0)]
    [(assoc memory param-result result) (+ pc 4) input output]))


(defn run-program
  ([memory pc input output]
   (if (> 0 pc)
     {:memory memory
      :pc pc
      :input input
      :output output}
     (let [[new-memory new-pc new-input new-output] (run-opcode memory pc input output)]
       (run-program new-memory new-pc new-input new-output))))
  ([memory input output]
   (run-program memory 0 input output))
  ([memory]
   (run-program memory [] [])))


(defn call-program
  [program noun verb input output]
  (-> program
      (assoc 1 noun)
      (assoc 2 verb)
      (run-program input output)))


(comment

  (run-opcode [99 1 0] 0 [] [])
  ;; [[99 1 0] -1]
  (run-opcode [1 0 0 0 99] 0 [] [])
  ;; [(2 0 0 0 99) 4]
  (run-opcode [2 3 0 3 99] 0 [] [])
  ;; [(2 3 0 6 99) 4]
  (run-opcode [3 0 99] 0 [1] [])
  ;; [[1 0 99] 2 () []]
  (run-opcode [4 0 99] 0 [] [])
  ;;[[4 0 99] 2 [] [4]]
  (run-opcode [1002 4 3 4 33] 0 [] [])
  ;; [[1002 4 3 4 99] 4 [] []]
  (run-opcode [1001 4 3 4 33] 0 [] [])
  ;; [[1001 4 3 4 36] 4 [] []]
  (run-opcode [104 4] 0 [] [])
  ;; [[104 4] 2 [] [4]]
  (run-opcode [108 4 3 4] 0 [] [])
  ;; [[108 4 3 4 1] 4 [] []]
  (run-opcode [8 1 2 4 1] 0 [] [])
  ;; [[8 1 2 4 0] 4 [] []]
  (run-opcode [107 4 3 4] 0 [] [])
  ;; [[107 4 3 4 0] 4 [] []]
  (run-opcode [7 1 2 4 0] 0 [] [])
  ;; [[7 1 2 4 1] 4 [] []]
  (run-opcode [1105 1 3] 0 [] [])
  ;; [[1105 1 3] 3 [] []]
  (run-opcode [1105 0 3] 0 [] [])
  ;; [[1105 0 3] 0 [] []]
  (run-opcode [1106 0 3] 0 [] [])
  ;; [[1106 0 3] 3 [] []]
  (run-opcode [1106 1 3] 0 [] [])
  ;; [[1106 1 3] 0 [] []]

  (run-program [99])
  ;; [99]
  (run-program [1 0 0 0 99])
  ;; (2 0 0 0 99)
  (run-program [2 3 0 3 99])
  ;; (2 3 0 6 99)
  (run-program [2 4 4 5 99 0])
  ;; (2 4 4 5 99 9801)
  (run-program [1 1 1 4 99 5 6 0 99])
  ;; (30 1 1 4 2 5 6 0 99)
  (run-program [3 0 4 0 99] [42] [])
  ;; {:memory [42 0 4 0 99], :pc -1, :input [], :output [42]}
  (run-program [1002 4 3 4 33] [] [])
  ;; {:memory [1002 4 3 4 99], :pc -1, :input [], :output []}
  (run-program [3 9 8 9 10 9 4 9 99 -1 8] [8] [])
  ;; {:memory [3 9 8 9 10 9 4 9 99 1 8], :pc -1, :input [], :output [1]}
  (run-program [3 9 7 9 10 9 4 9 99 -1 8] [7] [])
  ;; {:memory [3 9 7 9 10 9 4 9 99 1 8], :pc -1, :input [], :output [1]}
  (run-program [3 3 1108 -1 8 3 4 3 99] [7] [])
  ;; {:memory [3 3 1108 0 8 3 4 3 99], :pc -1, :input [], :output [0]}
  (run-program [3 3 1107 -1 8 3 4 3 99] [8] [])
  ;; {:memory [3 3 1107 0 8 3 4 3 99], :pc -1, :input [], :output [0]}
  (run-program [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] [0] [])
  ;; {:memory [3 12 6 12 15 1 13 14 13 4 13 99 0 0 1 9],
  ;;  :pc -1, 
  ;;  :input [], 
  ;;  :output [0]}
  (run-program [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] [8] [])
  ;; {:memory [3 12 6 12 15 1 13 14 13 4 13 99 8 1 1 9],
  ;;  :pc -1, 
  ;;  :input [], 
  ;;  :output [1]}
  (run-program [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] [0] [])
  ;; {:memory [3 3 1105 0 9 1101 0 0 12 4 12 99 0],
  ;;  :pc -1, 
  ;;  :input [], 
  ;;  :output [0]}
  (run-program [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] [8] [])
  ;; {:memory [3 3 1105 8 9 1101 0 0 12 4 12 99 1],
  ;;  :pc -1, 
  ;;  :input [], 
  ;;  :output [1]}
  (->
    (run-program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                  1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                  999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] [8] [])
    :output
    last)
  ;; 1000
  (->
    (run-program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                  1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                  999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] [9] [])
    :output
    last)
  ;; 1001
  (->
    (run-program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                  1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                  999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] [7] [])
    :output
    last)
  ;; 999

  (def gravity-program
    (mapv
      #(js/parseInt % 10)
      (clojure.string/split
        (fs/readFileSync "./gravity_program.txt")
        #",")))

  (-> gravity-program
      (call-program 12 2)
      :memory
      first)
  ;; 4570637

  (filter
    #(= 19690720 (nth % 2))
    (for [noun (range 99)
          verb (range 99)]
      [noun verb (-> gravity-program (call-program noun verb) :memory first)]))
  ;; ([54 85 19690720])

  (def test-program
    (mapv
      #(js/parseInt % 10)
      (clojure.string/split
        (fs/readFileSync "./test_program.txt")
        #",")))

  (-> test-program
      (run-program [1] [])
      :output
      last)
  ;; 11193703
  (-> test-program
      (run-program [5] [])
      :output
      last)
  ;; 12410607

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


(defn valid-password?
  [password]
  (let [digits (-> password .toString (.split "") js->clj)]
    ((every-pred #(= 6 (count %))
                 #(= (sort %) %)
                 #(not= (count (set %)) (count %))
                 (fn [d]
                   (->> d
                        (partition-by identity)
                        (filter #(= 2 (count %)))
                        first)))
     digits)))


(comment

  (vector
    (valid-password? 111111)
    (valid-password? 223450)
    (valid-password? 123789)
    (valid-password? 112233)
    (valid-password? 123444)
    (valid-password? 111122))

  (count (filter valid-password? (range 231832 767346)))
  )


(defn main [& cli-args]
  (prn "hello world")
  ;; (prn
  ;;   (wires->closest-intersection
  ;;     (map
  ;;       #(-> %
  ;;            (clojure.string/split ","))
  ;;       (clojure.string/split-lines
  ;;         (fs/readFileSync "./wires.txt")))))
  )
