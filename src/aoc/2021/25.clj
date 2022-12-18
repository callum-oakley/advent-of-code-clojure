(ns aoc.2021.25
  (:require
   [aoc.grid :as grid]
   [aoc.vector :refer [+v]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [g (grid/parse s)]
    [(+v [1 1] (last (sort (keys g))))
    (reduce-kv (fn [[east south] pos c]
                 (case c
                   \> [(conj east pos) south]
                   \v [east (conj south pos)]
                   \. [east south]))
               [#{} #{}]
               g)]))

(defn move [east south from to]
  (if (or (east to) (south to)) from to))

(defn part-1 [[[height width] [east south]]]
  (loop [east east south south steps 0]
    (let [east* (set (map (fn [[y x]]
                            (move east south [y x] [y (mod (inc x) width)]))
                          east))
          south* (set (map (fn [[y x]]
                             (move east* south [y x] [(mod (inc y) height) x]))
                           south))]
      (if (and (= east east*) (= south south*))
        (inc steps)
        (recur east* south* (inc steps))))))

(deftest test-example
  (is (= 58 (part-1 (parse (str/join "\n" ["v...>>.vv>" ".vv>>.vv.."
                                           ">>.>v>...v" ">>v>>.>.v."
                                           "v>v.vv.v.." ">.>>..v..."
                                           ".vv..>.>v." "v.v..>>v.v"
                                           "....v..v.>"]))))))
