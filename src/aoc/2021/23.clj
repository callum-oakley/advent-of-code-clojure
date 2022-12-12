(ns aoc.2021.23
  (:require
   [aoc.grid :as grid]
   [aoc.search :as search]
   [aoc.vector :refer [manhattan-distance]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (update-vals (->> s grid/parse (remove (comp #{\# \space} val)) (into {}))
               #(case % \. nil %)))

(defn unfold [s]
  (let [[top bottom] (->> s str/split-lines (split-at 3))]
    (str/join "\n" (concat top ["  #D#C#B#A#  " "  #D#B#A#C#  "] bottom))))

(def hall
  #{[1 1] [1 2] [1 4] [1 6] [1 8] [1 10] [1 11]})

(def room-x
  {\A 3 \B 5 \C 7 \D 9})

(defn room [room-height amphipod]
  (map (fn [y] [y (room-x amphipod)]) (range (inc room-height) 1 -1)))

(def step-energy
  {\A 1 \B 10 \C 100 \D 1000})

(def path
  (memoize
   (fn [from to]
     (let [[[hall-y hall-x] [room-y room-x]] (sort [from to])
           [min-x max-x] (sort [hall-x room-x])]
       (concat (map (fn [x] [hall-y x]) (range min-x (inc max-x)))
               (map (fn [y] [y room-x]) (range 2 (inc room-y))))))))

(defn clear? [grid path]
  (= 1 (count (keep grid path))))

(defn vacancy [room-height grid amphipod]
  (when (->> amphipod (room room-height) (keep grid) (every? #{amphipod}))
    (->> amphipod (room room-height) (remove grid) first)))

(defn all-home? [room-height grid amphipod]
  (every? #(= amphipod (grid %)) (room room-height amphipod)))

(defn move [{grid :grid :as state} amphipod from to]
  (when (clear? grid (path from to))
    (-> state
        (update :grid assoc from nil to amphipod)
        (update :energy + (* (step-energy amphipod)
                             (manhattan-distance from to))))))

(defn moves [room-height {grid :grid :as state}]
  (apply concat
         (keep (fn [from]
                 (when-let [amphipod (grid from)]
                   (when-let [to (vacancy room-height grid amphipod)]
                     (move state amphipod from to))))
               hall)
         (keep (fn [from]
                 (when-let [amphipod (grid from)]
                   (keep (fn [to] (move state amphipod from to)) hall)))
               (mapcat #(room room-height %)
                       (remove #(all-home? room-height grid %) "ABCD")))))

(defn heuristic [_ grid]
  (first
   (reduce-kv (fn [[energy depth] [y x] amphipod]
                (if (and amphipod (not= (room-x amphipod) x))
                  [(+ energy (* (step-energy amphipod)
                                (+ (abs (- (room-x amphipod) x))
                                   y
                                   (depth amphipod))))
                   (update depth amphipod inc)]
                  [energy depth]))
              [0 {\A 0 \B 0 \C 0 \D 0}]
              grid)))

(defn part-* [room-height grid]
  (:energy (search/a* {:grid grid :energy 0}
                      #(moves room-height %)
                      :grid
                      (fn [{:keys [grid]}]
                        (every? #(all-home? room-height grid %) "ABCD"))
                      :energy
                      #(heuristic room-height (:grid %)))))

(defn part-1 []
  (->> "input/2021/23" slurp parse (part-* 2)))

(defn part-2 []
  (->> "input/2021/23" slurp unfold parse (part-* 4)))

(deftest test-example
  (let [sample "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#"]
    (is (= 12521 (part-* 2 (parse sample))))
    (is (= 44169 (part-* 4 (parse (unfold sample)))))))
