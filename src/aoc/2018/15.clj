(ns aoc.2018.15
  (:require
   [aoc.grid :as g]
   [aoc.search :as search]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (g/parse s #(case %
                \# nil
                \. {:type :empty}
                \G {:type :goblin :hp 200 :ap 3}
                \E {:type :elf :hp 200 :ap 3})))

(defn in-range? [cave pos target-type]
  (some #(= target-type (:type (cave %))) (g/adjacent pos)))

(defn path [cave pos target-type]
  (:path (search/dijkstra (juxt :dist :pos)
                          {:dist 0 :pos pos :path []}
                          (fn [state]
                            (keep #(when (= :empty (:type (cave %)))
                                     (-> state
                                         (update :dist inc)
                                         (assoc :pos %)
                                         (update :path conj %)))
                                  (sort (g/adjacent (:pos state)))))
                          #(in-range? cave (:pos %) target-type)
                          :pos)))

(defn move [cave pos target-type]
  (if-let [pos* (first (path cave pos target-type))]
    [(assoc cave pos {:type :empty} pos* (cave pos)) pos*]
    [cave pos]))

(defn attack [cave pos target-type]
  (if-let [target (some->> (g/adjacent pos)
                           (filter #(= target-type (:type (cave %))))
                           ;; min-key is last wins so reverse
                           seq sort reverse
                           (apply min-key #(:hp (cave %))))]
    (let [hp (- (:hp (cave target)) (:ap (cave pos)))]
      (if (pos? hp)
        (assoc-in cave [target :hp] hp)
        (assoc cave target {:type :empty})))
    cave))

(defn turn [cave pos target-type]
  (if (some #{target-type} (map :type (vals cave)))
    (let [[cave pos] (move cave pos target-type)]
      (assoc-in (attack cave pos target-type) [pos :gone?] true))
    (reduced (reduced cave))))

(defn round [cave]
  (reduce
   (fn [cave pos]
     (cond
       (or (= :empty (:type (cave pos))) (:gone? (cave pos))) cave
       (= :goblin (:type (cave pos))) (turn cave pos :elf)
       (= :elf (:type (cave pos))) (turn cave pos :goblin)))
   cave
   (sort (keys cave))))

(defn part-1* [cave]
  (loop [cave (round cave) rounds 0]
    (if (reduced? cave)
      (* rounds (apply + (keep :hp (vals @cave))))
      (recur (round (update-vals cave #(dissoc % :gone?))) (inc rounds)))))

(defn part-1 []
  (-> "input/2018/15" slurp parse part-1*))

(deftest test-examples
  (is (= 27730 (part-1* (parse ".G...\n...EG\n.#.#G\n..G#E\n....."))))
  (is (= 36334 (part-1* (parse "G..#E\nE#E.E\nG.##.\n...#E\n...E."))))
  (is (= 39514 (part-1* (parse "E..EG\n.#G.E\nE.##E\nG..#.\n..E#."))))
  (is (= 27755 (part-1* (parse "E.G#.\n.#G..\nG.#.G\nG..#.\n...E."))))
  (is (= 28944 (part-1* (parse ".E...\n.#..G\n.###.\nE#G#G\n...#G"))))
  (is (= 18740 (part-1* (parse "G......\n.E.#...\n..##..G\n...##..\n...#...\n.G...G.\n.....G.")))))