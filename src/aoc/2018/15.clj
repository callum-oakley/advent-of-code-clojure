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

;; The cost function is fiddly, here's everything it needs to cover:
;; - To move, the unit first considers the squares that are in range and
;;   determines which of those squares it could reach in the fewest steps.
;; - If multiple squares are in range and tied for being reachable in the fewest
;;   steps, the square which is first in reading order is chosen.
;; - If multiple steps would put the unit equally closer to its destination, the
;;   unit chooses the step which is first in reading order.
;; Missing either of the last two steps produces correct results for all the
;; examples, but fails on the puzzle proper...
(defn first-step [cave pos target-type]
  (:first-step
   (search/dijkstra {:dist 0 :pos pos}
                    (fn [state]
                      (keep (fn [step]
                              (when (= :empty (:type (cave step)))
                                (-> state
                                    (update :dist inc)
                                    (assoc :pos step)
                                    (update :first-step #(or % step)))))
                            (sort (g/adjacent (:pos state)))))
                    :pos
                    #(in-range? cave (:pos %) target-type)
                    (juxt :dist :pos :first-step))))

(defn move [cave pos target-type]
  (if-let [pos* (first-step cave pos target-type)]
    [(assoc cave pos {:type :empty} pos* (cave pos)) pos*]
    [cave pos]))

(defn attack [cave pos target-type]
  (if-let [target (some->> (g/adjacent pos)
                           (filter #(= target-type (:type (cave %))))
                           (sort-by (juxt #(:hp (cave %)) identity))
                           first)]
    (let [hp (- (:hp (cave target)) (:ap (cave pos)))]
      (if (pos? hp)
        (assoc-in cave [target :hp] hp)
        (if (< 3 (:ap (cave target)))
          (reduced (reduced :unacceptable))
          (assoc cave target {:type :empty}))))
    cave))

(defn turn [r cave pos target-type]
  (if (some #{target-type} (map :type (vals cave)))
    (let [[cave pos] (move cave pos target-type)
          cave (attack cave pos target-type)]
      (if (reduced? cave)
        cave
        (assoc-in cave [pos :gone?] true)))
    (reduced (reduced (* r (apply + (keep :hp (vals cave))))))))

(defn round [r cave]
  (reduce
   (fn [cave pos]
     (cond
       (or (= :empty (:type (cave pos))) (:gone? (cave pos))) cave
       (= :goblin (:type (cave pos))) (turn r cave pos :elf)
       (= :elf (:type (cave pos))) (turn r cave pos :goblin)))
   cave
   (sort (keys cave))))

(defn part-1 [cave]
  (loop [cave cave r 0]
    (if (reduced? cave)
      @cave
      (recur (round r (update-vals cave #(dissoc % :gone?))) (inc r)))))

(defn part-2 [cave]
  (->> (iterate inc 4)
       (map (fn [ap]
              (part-1 (update-vals
                       cave
                       #(if (= :elf (:type %)) (assoc % :ap ap) %)))))
       (remove #{:unacceptable})
       first))

(deftest test-examples
  (is (= 27730 (part-1 (parse ".G...\n...EG\n.#.#G\n..G#E\n....."))))
  (is (= 36334 (part-1 (parse "G..#E\nE#E.E\nG.##.\n...#E\n...E."))))
  (is (= 39514 (part-1 (parse "E..EG\n.#G.E\nE.##E\nG..#.\n..E#."))))
  (is (= 27755 (part-1 (parse "E.G#.\n.#G..\nG.#.G\nG..#.\n...E."))))
  (is (= 28944 (part-1 (parse ".E...\n.#..G\n.###.\nE#G#G\n...#G"))))
  (is (= 18740 (part-1 (parse "G......\n.E.#...\n..##..G\n...##..\n...#...\n.G...G.\n.....G."))))
  (is (= 4988 (part-2 (parse ".G...\n...EG\n.#.#G\n..G#E\n....."))))
  (is (= 31284 (part-2 (parse "E..EG\n.#G.E\nE.##E\nG..#.\n..E#."))))
  (is (= 3478 (part-2 (parse "E.G#.\n.#G..\nG.#.G\nG..#.\n...E."))))
  (is (= 6474 (part-2 (parse ".E...\n.#..G\n.###.\nE#G#G\n...#G"))))
  (is (= 1140 (part-2 (parse "G......\n.E.#...\n..##..G\n...##..\n...#...\n.G...G.\n.....G.")))))
