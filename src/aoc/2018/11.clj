(ns aoc.2018.11
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn cell [serial x y]
  (- (mod (quot (* (+ x 10) (+ serial (* (+ x 10) y))) 100) 10) 5))

;; https://en.wikipedia.org/wiki/Summed-area_table
(defn table [serial]
  (reduce (fn [table [x y]]
            (assoc table [x y]
                   (+ (cell serial x y)
                      (get table [x (dec y)] 0)
                      (get table [(dec x) y] 0)
                      (- (get table [(dec x) (dec y)] 0)))))
          {}
          (for [x (range 1 301) y (range 1 301)] [x y])))

(defn square [table x y size]
  (+ (get table [(dec x) (dec y)] 0)
     (get table [(+ x (dec size)) (+ y (dec size))] 0)
     (- (get table [(+ x (dec size)) (dec y)] 0))
     (- (get table [(dec x) (+ y (dec size))] 0))))

(defn part-* [p table]
  (apply max-key
         (fn [[x y size]] (square table x y size))
         ;; Larger squares tend towards a negative power since the average cell
         ;; power is -0.5, so restrict the search to squares smaller than 20.
         (for [size (case p 1 [3] 2 (range 1 20))
               x (range 1 (- 302 size))
               y (range 1 (- 302 size))]
           [x y size])))

(defn part-1 []
  (->> "input/2018/11" slurp read-string table (part-* 1) butlast (str/join ",")))

(defn part-2 []
  (->> "input/2018/11" slurp read-string table (part-* 2) (str/join ",")))

(deftest test-examples
  (is (= 4 (cell 8 3 5)))
  (is (= -5 (cell 57 122 79)))
  (is (= 0 (cell 39 217 196)))
  (is (= 4 (cell 71 101 153)))
  (is (= 29 (square (table 18) 33 45 3)))
  (is (= 30 (square (table 42) 21 61 3)))
  (is (= [33 45 3] (part-* 1 (table 18))))
  (is (= [21 61 3] (part-* 1 (table 42))))
  (is (= [90 269 16] (part-* 2 (table 18))))
  (is (= [232 251 12] (part-* 2 (table 42)))))
