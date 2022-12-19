(ns aoc.2022.16 
  (:require
   [aoc.search :as search]
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn distance [tunnels a b]
  (:dist (search/bfs
          {:pos a :dist 0}
          #(map (fn [pos] {:pos pos :dist (inc (:dist %))}) (tunnels (:pos %)))
          :pos
          #(= b (:pos %)))))

;; Construct a map of a -> b -> "the cost of moving from a to b and opening the
;; valve when we arrive".
(defn simplify [[tunnels valves]]
  [(reduce (fn [tunnels* [a b]]
             (let [cost (inc (distance tunnels a b))]
               (-> tunnels* (assoc-in [a b] cost) (assoc-in [b a] cost))))
           {}
           (comb/combinations (conj (keys valves) 'AA) 2))
   valves])

(defn parse [s]
  (simplify
   (reduce (fn [[tunnels valves] line]
             (let [[pos & dest] (map read-string (re-seq #"[A-Z]{2}" line))
                   fr (read-string (re-find #"\d+" line))]
               [(assoc tunnels pos dest)
                (cond-> valves (pos? fr) (assoc pos fr))]))
           [{} {}]
           (str/split-lines s))))

(defn part-* [workers time [tunnels valves]]
  (:pressure
   (search/branch-and-bound-max
    {:workers (vec (repeat workers {:pos 'AA :minute 0}))
     :pressure 0
     :valves valves}
    (fn [{:keys [workers pressure valves]}]
      (keep (fn [[dest cost w]]
              (when (valves dest)
                (let [minute (+ cost (:minute (workers w)))]
                  (when (< minute time)
                    {:workers (assoc workers w {:pos dest :minute minute})
                     :pressure (+ pressure (* (- time minute) (valves dest)))
                     :valves (dissoc valves dest)}))))
            (mapcat (fn [w] (map #(conj % w) (tunnels (:pos (workers w)))))
                    (range (count workers)))))
    (fn [state] (update state workers #(sort-by :minute %)))
    :pressure
    ;; Computes an upper bound on the pressure that we could ultimately release
    ;; from this state, by always choosing:
    ;;   - the worker with the most time remaining
    ;;   - the valve with the highest release value
    ;; and assuming the cost of opening that valve is the lowest cost remaining.
    (fn [{:keys [workers pressure valves]}]
      (if (seq valves)
        (let [[dest p] (apply max-key second valves)
              w (apply min-key #(:minute (workers %)) (range (count workers)))
              min-c (apply min (map (tunnels (:pos (workers w))) (keys valves)))
              minute (+ min-c (:minute (workers w)))]
          (if (< minute time)
            (recur {:workers (assoc workers w {:pos dest :minute minute})
                    :pressure (+ pressure (* (- time minute) p))
                    :valves (dissoc valves dest)})
            pressure))
        pressure)))))

(defn part-1 [input]
  (part-* 1 30 input))

(defn part-2 [input]
  (part-* 2 26 input))

(def example
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
   Valve BB has flow rate=13; tunnels lead to valves CC, AA
   Valve CC has flow rate=2; tunnels lead to valves DD, BB
   Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
   Valve EE has flow rate=3; tunnels lead to valves FF, DD
   Valve FF has flow rate=0; tunnels lead to valves EE, GG
   Valve GG has flow rate=0; tunnels lead to valves FF, HH
   Valve HH has flow rate=22; tunnel leads to valve GG
   Valve II has flow rate=0; tunnels lead to valves AA, JJ
   Valve JJ has flow rate=21; tunnel leads to valve II")

(deftest test-example
  (is (= 1651 (part-1 (parse example))))
  (is (= 1707 (part-2 (parse example)))))
