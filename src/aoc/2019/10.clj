(ns aoc.2019.10
  (:require
   [aoc.grid :as grid]
   [aoc.vector :refer [-v manhattan-distance]]
   [clojure.math :as math]
   [clojure.string :as str]
   [clojure.test :refer [deftest are is]]))

(defn parse [s]
  (->> s grid/parse (remove (comp #{\.} val)) keys))

(defn theta [[y x]]
  (- (math/atan2 x y)))

(defn count-visible [asteroids origin]
  (count (distinct (map #(theta (-v % origin)) (remove #{origin} asteroids)))))

(defn part-1 [asteroids]
  (apply max (map #(count-visible asteroids %) asteroids)))

(defn part-2 [asteroids]
  (let [origin (apply max-key #(count-visible asteroids %) asteroids)]
    (->> asteroids (remove #{origin}) (group-by #(theta (-v % origin)))
         (mapcat (fn [[theta asteroids]]
                   (map-indexed (fn [sweep asteroid] [asteroid [sweep theta]])
                                (sort-by #(manhattan-distance % origin)
                                         asteroids))))
         (sort-by second) (map first) (drop 199) first
         ((fn [[y x]] (+ (* 100 x) y))))))

(def large-example
  [".#..##.###...#######" "##.############..##." ".#.######.########.#"
   ".###.#######.####.#." "#####.##.#.##.###.##" "..#####..#.#########"
   "####################" "#.####....###.#.#.##" "##.#################"
   "#####.##.###..####.." "..######..##.#######" "####.##.####...##..#"
   ".#####..#.######.###" "##...#.##########..." "#.##########.#######"
   ".####.#.###.###.#.##" "....##.##.###..#####" ".#.#.###########.###"
   "#.#.#.#####.####.###" "###.##.####.##.#..##"])

(deftest test-examples
  (are [a b] (= a (part-1 (parse (str/join "\n" b))))
    8 [".#..#" "....." "#####" "....#" "...##"]
    33 ["......#.#." "#..#.#...." "..#######." ".#.#.###.." ".#..#....."
        "..#....#.#" "#..#....#." ".##.#..###" "##...#..#." ".#....####"]
    35 ["#.#...#.#." ".###....#." ".#....#..." "##.#.#.#.#" "....#.#.#."
        ".##..###.#" "..#...##.." "..##....##" "......#..." ".####.###."]
    41 [".#..#..###" "####.###.#" "....###.#." "..###.##.#" "##.##.#.#."
        "....###..#" "..#.#..#.#" "#..#.#.###" ".##...##.#" ".....#.#.."]
    210 large-example)
  (is (= 802 (part-2 (parse (str/join "\n" large-example))))))
