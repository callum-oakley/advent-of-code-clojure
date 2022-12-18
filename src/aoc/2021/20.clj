(ns aoc.2021.20
  (:require
   [aoc.grid :as grid]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [[algorithm image] (str/split s #"\n\n")
        algorithm (str/replace algorithm #"\s" "")
        image (str/replace image " " "")]
    [(->> (range (count algorithm)) (filter #(= \# (get algorithm %))) set)
     {:on (->> image grid/parse (keep (fn [[k v]] (when (= \# v) k))) set)}]))

(defn bin->int [bin]
  (read-string (apply str "2r" (map {false 0 true 1} bin))))

(defn enhance [algorithm {on :on off :off}]
  (let [on? (fn [pixel]
              (->> pixel grid/adjacent-9 sort
                   (map #(if on (contains? on %) (not (contains? off %))))
                   bin->int algorithm))]
    (if (and on (algorithm 0))
      {:off (->> (or on off) (mapcat grid/adjacent-9) set (remove on?) set)}
      {:on (->> (or on off) (mapcat grid/adjacent-9) set (filter on?) set)})))

(defn part-* [n [algorithm image]]
  (->> image (iterate #(enhance algorithm %)) (drop n) first :on count))

(defn part-1 [data]
  (part-* 2 data))

(defn part-2 [data]
  (part-* 50 data))

(def sample
  "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
   #..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
   .######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
   .#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
   .#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
   ...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
   ..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

   #..#.\n#....\n##..#\n..#..\n..###")

(deftest test-example
  (is (= 35 (part-1 (parse sample))))
  (is (= 3351 (part-2 (parse sample)))))
