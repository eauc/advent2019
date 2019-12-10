(ns advent2019.core
  (:require ["fs" :as fs]
            [clojure.set]
            [clojure.core.async
             :as async
             :refer [>! <! close! go go-loop chan buffer timeout to-chan]]))

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
  [memory pc _ output]
  (go
    (prn "opcode-99 close!")
    (close! output)
    [memory -1]))

(defmethod run-opcode 1
  [memory pc _ _]
  (go
    (let [[_ param-a param-b param-result & prog-rest] (drop pc memory)
          a (parameter pc 0 memory)
          b (parameter pc 1 memory)
          result (+ a b)]
      (prn "opcode-1" a b param-result result)
      [(assoc memory param-result result) (+ pc 4)])))

(defmethod run-opcode 2
  [memory pc _ _]
  (go
    (let [[_ param-a param-b param-result & prog-rest] (drop pc memory)
          a (parameter pc 0 memory)
          b (parameter pc 1 memory)
          result (* a b)]
      (prn "opcode-2" a b param-result result)
      [(assoc memory param-result result) (+ pc 4)])))

(defmethod run-opcode 3
  [memory pc input _]
  (go
    (let [[_ param-to & prog-rest] (drop pc memory)
          in-value (<! input)]
      (prn "opcode-3" param-to in-value)
      [(assoc memory param-to in-value) (+ pc 2)])))

(defmethod run-opcode 4
  [memory pc _ output]
  (go
    (let [[_ param-from & prog-rest] (drop pc memory)
          from (parameter pc 0 memory)]
      (prn "opcode-4" from)
      (>! output from)
      [memory (+ pc 2)])))

(defmethod run-opcode 5
  [memory pc _ _]
  (go
    (let [[_ param-a param-result & prog-rest] (drop pc memory)
          test-value (parameter pc 0 memory)
          jump-to (parameter pc 1 memory)
          new-pc (if (not= test-value 0) jump-to (+ pc 3))]
      (prn "opcode-5" test-value jump-to new-pc)
      [memory new-pc])))

(defmethod run-opcode 6
  [memory pc _ _]
  (go
    (let [[_ param-a param-result & prog-rest] (drop pc memory)
          test-value (parameter pc 0 memory)
          jump-to (parameter pc 1 memory)
          new-pc (if (= test-value 0) jump-to (+ pc 3))]
      (prn "opcode-6" test-value jump-to new-pc)
      [memory new-pc])))

(defmethod run-opcode 7
  [memory pc _ _]
  (go
    (let [[_ param-a param-b param-result & prog-rest] (drop pc memory)
          a (parameter pc 0 memory)
          b (parameter pc 1 memory)
          result (if (< a b) 1 0)]
      (prn "opcode-7" a b param-result result)
      [(assoc memory param-result result) (+ pc 4)])))

(defmethod run-opcode 8
  [memory pc _ _]
  (go
    (let [[_ param-a param-b param-result & prog-rest] (drop pc memory)
          a (parameter pc 0 memory)
          b (parameter pc 1 memory)
          result (if (= a b) 1 0)]
      (prn "opcode-8" a b param-result result)
      [(assoc memory param-result result) (+ pc 4)])))


(defn run-program
  ([memory pc input output]
   (prn "start")
   (go-loop [current-memory memory
             current-pc pc]
     (if (> 0 current-pc)
       (do
         (prn "end")
         {:memory current-memory
          :pc current-pc})
       (let [[new-memory new-pc] (<! (run-opcode current-memory current-pc input output))
             _ (prn (str "ran-opcode " current-pc "->" new-pc))]
         (recur new-memory new-pc)))))
  ([memory input output]
   (run-program memory 0 input output))
  ([memory]
   (run-program memory (chan) (chan))))


(defn call-program
  [program noun verb]
  (-> program
      (assoc 1 noun)
      (assoc 2 verb)
      run-program))


(comment

  (go
    (def *result nil)
    (def *result (<! (run-opcode [99 1 0] 0 (chan) (chan)))))
  ;; [[99 1 0] -1]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [1 0 0 0 99] 0 (chan) (chan)))))
  ;; [(2 0 0 0 99) 4]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [2 3 0 3 99] 0 (chan) (chan)))))
  ;; [(2 3 0 6 99) 4]
  (go
    (def *result nil)
    (let [_ (prn "start")
          input (to-chan [1])
          _ (prn "input")
          [memory pc] (<! (run-opcode [3 0 99] 0 input (chan)))]
      (def *result [memory pc])))
  ;; [[1 0 99] 2]
  (go
    (def *result nil)
    (let [output (chan 1)
          [memory pc] (<! (run-opcode [4 0 99] 0 (chan) output))]
      (def *result [memory pc (<! output)])))
  ;;[[4 0 99] 2 4]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [1002 4 3 4 33] 0 (chan) (chan)))))
  ;; [[1002 4 3 4 99] 4]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [1001 4 3 4 33] 0 (chan) (chan)))))
  ;; [[1001 4 3 4 36] 4]
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-opcode [104 4] 0 (chan) output))]
      (def *result (conj result (<! output)))))
  ;; [[104 4] 2 4]
  (go
    (let [output (chan 1)
          result (<! (run-opcode [104 5] 0 (chan) output))]
      (def *result (conj result (<! output)))))
  ;; [[104 4] 2 5]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [108 4 3 4] 0 (chan) (chan)))))
  ;; [[108 4 3 4 1] 4]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [8 1 2 4 1] 0 (chan) (chan)))))
  ;; [[8 1 2 4 0] 4]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [107 4 3 4] 0 (chan) (chan)))))
  ;; [[107 4 3 4 0] 4]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [7 1 2 4 0] 0 (chan) (chan)))))
  ;; [[7 1 2 4 1] 4]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [1105 1 5] 0 (chan) (chan)))))
  ;; [[1105 1 5] 5]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [1105 0 5] 0 (chan) (chan)))))
  ;; [[1105 0 5] 3]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [1106 0 5] 0 (chan) (chan)))))
  ;; [[1106 0 5] 5]
  (go
    (def *result nil)
    (def *result (<! (run-opcode [1106 1 5] 0 (chan) (chan)))))
  ;; [[1106 1 5] 3]

  (go
    (def *result nil)
    (def *result (<! (run-program [99]))))
  ;; {:memory [99], :pc -1}
  (go
    (def *result nil)
    (def *result (<! (run-program [1 0 0 0 99]))))
  ;; {:memory [2 0 0 0 99], :pc -1}
  (go
    (def *result nil)
    (def *result (<! (run-program [2 3 0 3 99]))))
  ;; {:memory [2 3 0 6 99], :pc -1}
  (go
    (def *result nil)
    (def *result (<! (run-program [2 4 4 5 99 0]))))
  ;; {:memory [2 4 4 5 99 9801], :pc -1}
  (go
    (def *result nil)
    (def *result (<! (run-program [1 1 1 4 99 5 6 0 99]))))
  ;; {:memory [30 1 1 4 2 5 6 0 99], :pc -1}
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 0 4 0 99] (to-chan [42]) output))]
      (def *result [result (<! output)])))
  ;; [{:memory [42 0 4 0 99], :pc -1} 42]
  (go
    (def *result nil)
    (def *result (<! (run-program [1002 4 3 4 33]))))
  ;; {:memory [1002 4 3 4 99], :pc -1}
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 9 8 9 10 9 4 9 99 -1 8] (to-chan [8]) output))]
      (def *result [result (<! output)])))
  ;; [{:memory [3 9 8 9 10 9 4 9 99 1 8], :pc -1} 1]
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 9 7 9 10 9 4 9 99 -1 8] (to-chan [7]) output))]
      (def *result [result (<! output)])))
  ;; [{:memory [3 9 7 9 10 9 4 9 99 1 8], :pc -1} 1]
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 3 1108 -1 8 3 4 3 99] (to-chan [7]) output))]
      (def *result [result (<! output)])))
  ;; [{:memory [3 3 1108 0 8 3 4 3 99], :pc -1} 0]
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 3 1107 -1 8 3 4 3 99] (to-chan [8]) output))]
      (def *result [result (<! output)])))
  ;; [{:memory [3 3 1107 0 8 3 4 3 99], :pc -1} 0]
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] (to-chan [0]) output))]
      (def *result [result (<! output)])))
  ;; [{:memory [3 12 6 12 15 1 13 14 13 4 13 99 0 0 1 9], :pc -1} 0]
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] (to-chan [8]) output))]
      (def *result [result (<! output)])))
  ;; [{:memory [3 12 6 12 15 1 13 14 13 4 13 99 8 1 1 9], :pc -1} 1]
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] (to-chan [0]) output))]
      (def *result [result (<! output)])))
  ;; [{:memory [3 3 1105 0 9 1101 0 0 12 4 12 99 0], :pc -1} 0]
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] (to-chan [8]) output))]
      (def *result [result (<! output)])))
  ;; [{:memory [3 3 1105 8 9 1101 0 0 12 4 12 99 1], :pc -1} 1]
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                                   1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                                   999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                                  (to-chan [8]) output))]
      (def *result (<! output))))
  ;; 1000
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                                   1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                                   999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                                  (to-chan [9]) output))]
      (def *result (<! output))))
  ;; 1001
  (go
    (def *result nil)
    (let [output (chan 1)
          result (<! (run-program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                                   1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                                   999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                                  (to-chan [7]) output))]
      (def *result (<! output))))
  ;; 999

  (def gravity-program
    (mapv
      #(js/parseInt % 10)
      (clojure.string/split
        (fs/readFileSync "./gravity_program.txt")
        #",")))

  (go
    (def *result nil)
    (def *result
      (-> (<! (call-program gravity-program 12 2))
          :memory
          first)))
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

  (go
    (def *result nil)
    (let [output (chan 1)
          _ (run-program test-program (to-chan [1]) output)]
      (def *result (last (<! (async/into [] output))))))
  ;; 11193703

  (go
    (def *result nil)
    (let [output (chan 1)
          _ (run-program test-program (to-chan [5]) output)]
      (def *result (last (<! (async/into [] output))))))
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


(defn orbit-pairs
  [orbits]
  (mapv #(clojure.string/split % ")") orbits))


(defn object-center
  [object orbit-pairs]
  (first (first (filter #(= (last %) object) orbit-pairs))))


(defn orbit-checksum
  [checksums object orbit-pairs]
  (if (get checksums object)
    checksums
    (if (= object "COM")
      (assoc checksums "COM" 0)
      (let [center (object-center object orbit-pairs)
            new-checksums (orbit-checksum checksums center orbit-pairs)
            center-checksum (get new-checksums center)]
        (assoc new-checksums object (inc center-checksum))))))


(defn orbits-checksum
  [orbits]
  (let [pairs (orbit-pairs orbits)]
    (->> pairs
         (mapv last)
         (reduce #(orbit-checksum %1 %2 pairs) {})
         (map last)
         (reduce + 0))))


(defn object-centers
  [object orbit-pairs]
  (loop [current-object object
         centers []]
    (let [center (object-center current-object orbit-pairs)]
      (if (= center "COM")
        (conj centers "COM")
        (recur center (conj centers center))))))

(defn centers-distance
  [from centers]
  (count
    (take-while #(not= from %) centers)))


(defn orbital-transferts
  [from to orbits]
  (let [pairs (orbit-pairs orbits)
        from-centers (object-centers from pairs)
        to-centers (object-centers to pairs)
        common-centers (clojure.set/intersection
                         (set from-centers)
                         (set to-centers))
        closest-common (->> common-centers
                            (sort-by #(centers-distance % from-centers))
                            first)]
    (+ (centers-distance closest-common from-centers)
       (centers-distance closest-common to-centers))))


(comment

  (orbits-checksum
    ["COM)B"
     "B)C"
     "C)D"
     "D)E"
     "E)F"
     "B)G"
     "G)H"
     "D)I"
     "E)J"
     "J)K"
     "K)L"])

  (orbital-transferts
    "YOU" "SAN"
    ["COM)B"
     "B)C"
     "C)D"
     "D)E"
     "E)F"
     "B)G"
     "G)H"
     "D)I"
     "E)J"
     "J)K"
     "K)L"
     "K)YOU"
     "I)SAN"])

  (def orbits
    (clojure.string/split-lines
      (fs/readFileSync "./orbits.txt")))

  (orbits-checksum orbits)
  ;; 158090

  (orbital-transferts "YOU" "SAN" orbits)
  ;; 241

  )


(defn amplifier-chain
  [program [p1 p2 p3 p4 p5] output]
  (let [c1 (to-chan [p1 0])
        c2 (chan 1)
        c3 (chan 1)
        c4 (chan 1)
        c5 (chan 1)]
    (go
      (>! c2 p2)
      (>! c3 p3)
      (>! c4 p4)
      (>! c5 p5))
    (run-program program c5 output)
    (run-program program c4 c5)
    (run-program program c3 c4)
    (run-program program c2 c3)
    (run-program program c1 c2)))


(def phases
  (for [p1 (range 5)
        p2 (filter (complement #{p1}) (range 5))
        p3 (filter (complement #{p1 p2}) (range 5))
        p4 (filter (complement #{p1 p2 p3}) (range 5))
        p5 (filter (complement #{p1 p2 p3 p4}) (range 5))]
    [p1 p2 p3 p4 p5]))

(defn amplifier-chain-max-output
  [program]
  (go-loop [[current-phases & phases-rest] phases
            result 0]
    (if (nil? current-phases)
      result
      (let [output (chan 1)
            _ (amplifier-chain program current-phases output)
            new-result (<! output)]
        (recur phases-rest (max result new-result))))))


(defn amplifier-loop
  [program [p1 p2 p3 p4 p5]]
  (let [c1 (chan 1)
        c1m (async/mult c1)
        c2 (chan 1)
        c3 (chan 1)
        c4 (chan 1)
        c5 (chan 1)]
    (go
      (prn "c1")
      (>! c1 p1)
      (>! c2 p2)
      (>! c3 p3)
      (>! c4 p4)
      (>! c5 p5)
      (>! c1 0)
      (prn "all"))
    (run-program program c5 c1)
    (run-program program c4 c5)
    (run-program program c3 c4)
    (run-program program c2 c3)
    (run-program program (async/tap c1m (chan 1)) c2)
    (async/tap c1m (chan 1))))


(def loop-phases
  (for [p1 (range 5 10)
        p2 (filter (complement #{p1}) (range 5 10))
        p3 (filter (complement #{p1 p2}) (range 5 10))
        p4 (filter (complement #{p1 p2 p3}) (range 5 10))
        p5 (filter (complement #{p1 p2 p3 p4}) (range 5 10))]
    [p1 p2 p3 p4 p5]))

(defn amplifier-loop-max-output
  [program]
  (go-loop [[current-phases & phases-rest] loop-phases
            result 0]
    (prn current-phases result)
    (if (nil? current-phases)
      result
      (let [output (amplifier-loop program current-phases)
            new-result (last (<! (async/into [] output)))]
        (prn phases-rest new-result)
        (recur phases-rest (max result new-result))))))


(comment

  (def amplifier-program
    (mapv
      #(js/parseInt % 10)
      (clojure.string/split
        (fs/readFileSync "./amplifier_program.txt")
        #",")))

  (go
    (def *result nil)
    (let [output (chan 1)]
      (run-program amplifier-program (to-chan [0 0]) output)
      (def *result (<! output))))
  ;; 18

  (go
    (def *result nil)
    (let [output (chan 1)]
      (amplifier-chain
        [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0]
        [4 3 2 1 0]
        output)
      (def *result (<! output))))
  ;; 43210

  (do
    (def *result nil)
    (go
      (def *result
        (<! (amplifier-chain-max-output
              [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])))))
  ;; 43210

  (do
    (def *result nil)
    (go
      (def *result
        (<! (amplifier-chain-max-output
              [3 23 3 24 1002 24 10 24 1002 23 -1 23
               101 5 23 23 1 24 23 23 4 23 99 0 0])))))
  ;; 54321

  (do
    (def *result nil)
    (go
      (def *result
        (<! (amplifier-chain-max-output
              [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33
               1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0])))))
  ;; 65210

  (do
    (def *result nil)
    (go
      (def *result
        (<! (amplifier-chain-max-output
              amplifier-program)))))
  ;; 87138

  (go
    (def *result nil)
    (let [output ;; (chan 1)
          (amplifier-loop
            [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26
             27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5]
            [9 8 7 6 5])]
      (def *result (last (<! (async/into [] output))))))
  ;; 139629729

  (go
    (def *result nil)
    (let [output ;; (chan 1)
          (amplifier-loop
            [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54
             -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4
             53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10]
            [9 7 8 5 6])]
      (def *result (last (<! (async/into [] output))))))
  ;; 18216

  (do
    (def *result nil)
    (go
      (def *result
        (<! (amplifier-loop-max-output
              [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26
               27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5])))))
  ;; 139629729

  (do
    (def *result nil)
    (go
      (def *result
        (<! (amplifier-loop-max-output
              [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54
               -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4
               53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10])))))
  ;; 18216

  (do
    (def *result nil)
    (go
      (def *result
        (<! (amplifier-loop-max-output
              amplifier-program)))))
  ;; 17279674

  )


(defn img-layers
  [img-str width height]
  (partition-all (* width height) img-str))


(defn img-check
  [img-str width height]
  (let [layers (img-layers img-str width height)
        check-layer (->> layers
                         (sort-by #(count (filter #{"0"} %)))
                         first)]
    (* (count (filter #{"1"} check-layer))
       (count (filter #{"2"} check-layer)))))


(defn decoded-img
  [img-str width height]
  (apply
    map (fn [& pixels]
          (first (filter (complement #{"2"}) pixels)))
    (img-layers img-str width height)))


(comment

  (img-check "123456789012" 3 2)

  (def img
    (butlast
      (.toString
        (fs/readFileSync "./image.txt"))))

  (img-check img 25 6)
  ;; 2193


  (decoded-img "0222112222120000" 2 2)

  (println
    (clojure.string/join
      "\n"
      (map
        #(clojure.string/join "" %)
        (partition-all
          25
          (map
            #(if (= "0" %) " " "*")
            (decoded-img img 25 6))))))

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
