(ns aoc.2021.04
  (:require
   [aoc.vector :refer [transpose]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [[ns & boards] (str/split s #"\n\n")]
    [(->> ns (re-seq #"\d+") (map read-string))
     (map #(->> % (re-seq #"\d+") (map read-string) (partition 5)) boards)]))

(defn bingo? [board called]
  (some #(every? called %) (concat board (transpose board))))

(defn play [ns board]
  (loop [ns ns called #{} called-last nil moves 0]
    (if (bingo? board called)
      {:moves moves
       :score (->> board flatten (remove called) (apply +) (* called-last))}
      (recur (rest ns) (conj called (first ns)) (first ns) (inc moves)))))

(defn part-1 [[ns boards]]
  (->> boards (map #(play ns %)) (apply min-key :moves) :score))

(defn part-2 [[ns boards]]
  (->> boards (map #(play ns %)) (apply max-key :moves) :score))

(deftest test-example
  (let [sample "7,4,9,5,11,17,23,2,0,14,21,24,10,16,
                13,6,15,25,12,22,18,20,8,19,3,26,1

                22 13 17 11 0 8 2 23 4 24 21 9 14 16 7
                6 10 3 18 5 1 12 20 15 19

                3 15 0 2 22 9 18 13 17 5 19 8 7 25 23
                20 11 10 24 4 14 21 16 12 6

                14 21 17 24 4 10 16 15 9 19 18 8 23 26 20
                22 11 13 6 5 2 0 12 3 7"]
    (is (= 4512 (part-1 (parse sample))))
    (is (= 1924 (part-2 (parse sample))))))
