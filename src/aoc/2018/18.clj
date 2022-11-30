(ns aoc.2018.18
  (:require
   [aoc.grid :as grid]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (grid/parse s #(case % \. :open \| :trees \# :lumberyard)))

(defn tick [state]
  (reduce-kv
   (fn [state* pos type]
     (let [adjacent (map state (grid/adjacent-8 pos))]
       (case type
         :open (if (<= 3 (count (filter #{:trees} adjacent)))
                 (assoc state* pos :trees)
                 state*)
         :trees (if (<= 3 (count (filter #{:lumberyard} adjacent)))
                  (assoc state* pos :lumberyard)
                  state*)
         :lumberyard (if (and (some #{:trees} adjacent)
                              (some #{:lumberyard} adjacent))
                       state*
                       (assoc state* pos :open)))))
   state
   state))

(defn part-* [mins state]
  (let [state (->> state (iterate tick) (drop mins) first)]
    (* (count (filter #{:trees} (vals state)))
       (count (filter #{:lumberyard} (vals state))))))

(defn find-cycle [state]
  (loop [mins 0 state state seen {}]
    (if (seen state)
      [(seen state) mins]
      (recur (inc mins) (tick state) (assoc seen state mins)))))

(defn part-1 []
  (->> "input/2018/18" slurp parse (part-* 10)))

(defn part-2 []
  (let [state (parse (slurp "input/2018/18"))
        [start end] (find-cycle state)]
    (part-* (+ start (mod (- 1000000000 start) (- end start))) state)))

(def example
  (str/join "\n" [".#.#...|#." ".....#|##|" ".|..|...#." "..|#.....#"
                  "#.#|||#|#|" "...#.||..." ".|....|..." "||...#|.#|"
                  "|.||||..|." "...#.|..|."]))

(deftest test-example
  (is (= 1147 (part-* 10 (parse example)))))