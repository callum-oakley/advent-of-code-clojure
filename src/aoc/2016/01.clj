(ns aoc.2016.01
  (:require
   [aoc.vectors :refer [+v *v]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> (re-seq #"(R|L)(\d+)" s)
       (map (fn [[_ turn steps]] [(symbol turn) (read-string steps)]))))

(defn turn [state t]
  (case t
    L (update state :dir (fn [[x y]] [(- y) x]))
    R (update state :dir (fn [[x y]] [y (- x)]))))

(defn walk [state steps]
  (update state :path into (map #(+v (-> state :path peek) (*v % (:dir state)))
                                 (range 1 (inc steps)))))

(defn path [instructions]
  (:path (reduce (fn [state [t steps]] (-> state (turn t) (walk steps)))
                  {:dir [0 1] :path [[0 0]]}
                  instructions)))

(defn manhattan-distance [pos]
  (apply + (map #(Math/abs %) pos)))

(defn part-1* [instructions]
  (manhattan-distance (peek (path instructions))))

(defn part-2* [instructions]
  (manhattan-distance (reduce (fn [seen pos]
                                (if (seen pos)
                                  (reduced pos)
                                  (conj seen pos)))
                              #{}
                              (path instructions))))

(defn part-1 []
  (->> "input/2016/01" slurp parse part-1*))

(defn part-2 []
  (->> "input/2016/01" slurp parse part-2*))

(deftest test-part-1*
  (is (= 5 (part-1* (parse "R2, L3"))))
  (is (= 2 (part-1* (parse "R2, R2, R2"))))
  (is (= 12 (part-1* (parse "R5, L5, R5, R3")))))

(deftest test-part-2*
  (is (= 4 (part-2* (parse "R8, R4, R4, R8")))))

(deftest test-answers
  (is (= 252 (part-1)))
  (is (= 143 (part-2))))
