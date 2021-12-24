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

(defn room [grid amphipod]
  (->> (keys grid)
       (filter (fn [[y x]]
                 (and (< 1 y) (= (case amphipod \A 3 \B 5 \C 7 \D 9) x))))
       (sort #(compare %2 %1))))

(def step-energy
  {\A 1 \B 10 \C 100 \D 1000})

(defn path [from to]
  (let [[[hall-y hall-x] [room-y room-x]] (sort [from to])
        [min-x max-x] (sort [hall-x room-x])]
    (concat (map (fn [x] [hall-y x]) (range min-x (inc max-x)))
            (map (fn [y] [y room-x]) (range 2 (inc room-y))))))

(defn clear? [grid path]
  (= 1 (count (keep grid path))))

(defn vacancy [grid amphipod]
  (when (->> amphipod (room grid) (keep grid) (every? #{amphipod}))
    (->> amphipod (room grid) (remove grid) first)))

(defn all-home? [grid amphipod]
  (every? #(= amphipod (grid %)) (room grid amphipod)))

(defn move [{grid :grid :as state} amphipod from to]
  (when (clear? grid (path from to))
    (-> state
        (update :grid assoc from nil to amphipod)
        (update :energy + (* (step-energy amphipod)
                             (manhattan-distance from to))))))

(defn moves [{grid :grid :as state}]
  (apply concat
         (keep (fn [from]
                 (when-let [amphipod (grid from)]
                   (when-let [to (vacancy grid amphipod)]
                     (move state amphipod from to))))
               hall)
         (keep (fn [from]
                 (when-let [amphipod (grid from)]
                   (keep (fn [to] (move state amphipod from to)) hall)))
               (mapcat #(room grid %) (remove #(all-home? grid %) "ABCD")))))

(defn part-* [grid]
  (:energy (search/dijkstra :energy
                            {:grid grid :energy 0}
                            moves
                            (fn [{:keys [grid]}]
                              (every? #(all-home? grid %) "ABCD"))
                            :grid)))

(defn part-1 []
  (->> "input/2021/23" slurp parse part-*))

(defn part-2 []
  (->> "input/2021/23" slurp unfold parse part-*))

(deftest test-example
  (let [sample "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#"]
    (is (= 12521 (part-* (parse sample))))
    (is (= 44169 (part-* (parse (unfold sample)))))))
