(ns aoc.2022.02
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\w") (map read-string) (partition 2)))

(def beats
  '{R S P R S P})

(defn response->outcome [opponent response]
  (cond
    (= (beats opponent) response) 'L
    (= opponent response) 'D
    (= opponent (beats response)) 'W))

(defn outcome->response [opponent outcome]
  (case outcome
    L (beats opponent)
    D opponent
    W ((set/map-invert beats) opponent)))

(defn score [rounds]
  (apply + (map '{R 1 P 2 S 3 L 0 D 3 W 6} (flatten rounds))))

(defn part-1* [strategy]
  (->> strategy
       (map #(map '{A R B P C S X R Y P Z S} %))
       (map (fn [[opponent response]]
          [response (response->outcome opponent response)]))
       score))

(defn part-2* [strategy]
  (->> strategy
       (map #(map '{A R B P C S X L Y D Z W} %))
       (map (fn [[opponent outcome]]
          [(outcome->response opponent outcome) outcome]))
       score))

(defn part-1 []
  (->> "input/2022/02" slurp parse part-1*))

(defn part-2 []
  (->> "input/2022/02" slurp parse part-2*))

(deftest test-example
  (is (= 15 (part-1* (parse "A Y B X C Z"))))
  (is (= 12 (part-2* (parse "A Y B X C Z")))))
