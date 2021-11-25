(ns aoc.2016.02
  (:require
   [aoc.vectors :refer [+v]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def dir->vec
  {\U [0 -1] \D [0 1] \L [-1 0] \R [1 0]})

(def keypad-1
  {[0 0] \1 [1 0] \2 [2 0] \3
   [0 1] \4 [1 1] \5 [2 1] \6
   [0 2] \7 [1 2] \8 [2 2] \9})

(def keypad-2
  {                  [2 0] \1
            [1 1] \2 [2 1] \3 [3 1] \4
   [0 2] \5 [1 2] \6 [2 2] \7 [3 2] \8 [4 2] \9
            [1 3] \A [2 3] \B [3 3] \C
                     [2 4] \D})

(defn part-* [keypad instructions]
  (->> instructions
       (reductions (fn [pos instruction]
                     (reduce (fn [pos dir]
                               (let [pos* (+v pos (dir->vec dir))]
                                 (if (contains? keypad pos*) pos* pos)))
                             pos
                             instruction))
                   ((set/map-invert keypad) \5))
       rest
       (map keypad)
       (apply str)))

(defn part-1 []
  (->> "input/2016/02" slurp str/split-lines (part-* keypad-1)))

(defn part-2 []
  (->> "input/2016/02" slurp str/split-lines (part-* keypad-2)))

(deftest test-part-*
  (is (= "1985" (part-* keypad-1 ["ULL" "RRDDD" "LURDL" "UUUUD"])))
  (is (= "5DB3" (part-* keypad-2 ["ULL" "RRDDD" "LURDL" "UUUUD"]))))
