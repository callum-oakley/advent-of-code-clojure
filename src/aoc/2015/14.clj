(ns aoc.2015.14
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map #(let [[speed fly-duration rest-duration]
              (map read-string (re-seq #"\d+" %))]
          {:speed speed
           :fly-duration fly-duration
           :rest-duration rest-duration
           :state :fly
           :timer fly-duration
           :distance 0
           :points 0})
       (str/split-lines s)))

(defn update-distance [r]
  (if (= :fly (:state r))
    (update r :distance #(+ (:speed r) %))
    r))

(defn update-timer [r]
  (update r :timer dec))

(defn update-state [r]
  (if (zero? (:timer r))
    (case (:state r)
      :fly (assoc r :state :rest :timer (:rest-duration r))
      :rest (assoc r :state :fly :timer (:fly-duration r)))
    r))

(defn update-points [rs]
  (let [farthest (apply max (map :distance rs))]
    (map #(if (= farthest (:distance %))
            (update % :points inc)
            %)
         rs)))

(defn tick [rs]
  (update-points (map (comp update-state update-timer update-distance) rs)))

(defn part-* [target duration rs]
  (apply max (map target (nth (iterate tick rs) duration))))

(defn part-1 []
  (->> "input/2015/14" slurp parse (part-* :distance 2503)))

(defn part-2 []
  (->> "input/2015/14" slurp parse (part-* :points 2503)))

(deftest test-part-*
  (is (= 1120 (part-* :distance 1000 (parse "14 10 127\n16 11 162"))))
  (is (= 689 (part-* :points 1000 (parse "14 10 127\n16 11 162")))))

(deftest test-answers
  (is (= 2660 (part-1)))
  (is (= 1256 (part-2))))
