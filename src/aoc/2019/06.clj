(ns aoc.2019.06
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\w+") (partition 2) (map (comp vec reverse)) (into {})))

(defn orbits* [orbits obj]
  (if-let [o (orbits obj)]
    (conj (orbits* orbits o) o)
    #{}))

(defn part-1 [orbits]
  (->> orbits keys (map #(count (orbits* orbits %))) (apply +)))

(defn symmetric-diff [a b]
  (set/difference (set/union a b) (set/intersection a b)))

(defn part-2 [orbits]
  (count (symmetric-diff (orbits* orbits "YOU") (orbits* orbits "SAN"))))

(deftest test-example
  (is (= 42 (part-1 (parse "COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L"))))
  (is (= 4 (part-2 (parse "COM)B B)C C)D D)E E)F B)G G)H
                           D)I E)J J)K K)L K)YOU I)SAN")))))
