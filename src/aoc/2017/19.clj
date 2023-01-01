(ns aoc.2017.19
  (:require
   [aoc.grid :as grid]
   [aoc.vector :refer [+v]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (vec (str/split-lines s)))

(defn follow-route [diagram]
  (loop [pos [0 (str/index-of (first diagram) \|)]
         dir grid/south
         route [(get-in diagram pos)]]
    (if-let [[pos dir] (some (fn [dir]
                               (let [pos (+v pos dir)]
                                 (when (not= \space (get-in diagram pos))
                                   [pos dir])))
                             [dir (grid/left dir) (grid/right dir)])]
      (recur pos dir (conj route (get-in diagram pos)))
      route)))

(defn part-1 [diagram]
  (->> diagram follow-route (remove #{\| \- \+}) (apply str)))

(defn part-2 [diagram]
  (->> diagram follow-route count))

(deftest test-example
  (let [example ["     |          "
                 "     |  +--+    "
                 "     A  |  C    "
                 " F---|----E|--+ "
                 "     |  |  |  D "
                 "     +B-+  +--+ "
                 "                "]]
    (is (= "ABCDEF" (apply str (remove #{\| \- \+} (follow-route example)))))
    (is (= 38 (count (follow-route example))))))
