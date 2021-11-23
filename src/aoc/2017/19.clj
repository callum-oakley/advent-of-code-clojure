(ns aoc.2017.19
  (:require
   [aoc.vectors :refer [+v]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def left
  {[0 1] [-1 0] [-1 0] [0 -1] [0 -1] [1 0] [1 0] [0 1]})

(def right
  (set/map-invert left))

(defn follow-route [diagram]
  (loop [pos [0 (str/index-of (first diagram) \|)]
         dir [1 0]
         route [(get-in diagram pos)]]
    (if-let [[pos dir] (some (fn [dir]
                               (let [pos (+v pos dir)]
                                 (when (not= \space (get-in diagram pos))
                                   [pos dir])))
                             [dir (left dir) (right dir)])]
      (recur pos dir (conj route (get-in diagram pos)))
      route)))

(defn part-1 []
  (->> "input/2017/19" slurp str/split-lines vec follow-route
       (remove #{\| \- \+}) (apply str)))

(defn part-2 []
  (->> "input/2017/19" slurp str/split-lines vec follow-route count))

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

(deftest test-answers
  (is (= "HATBMQJYZ" (part-1)))
  (is (= 16332 (part-2))))
