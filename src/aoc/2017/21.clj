(ns aoc.2017.21
  (:require
   [aoc.vector :refer [transpose]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn rotate [pattern]
  (transpose (map reverse pattern)))

(defn symmetries [pattern]
  (concat (take 4 (iterate rotate pattern))
          (take 4 (iterate rotate (transpose pattern)))))

(defn parse-pattern [s]
  (vec (map vec (str/split s #"/"))))

(defn parse [s]
  (reduce (fn [rules line]
            (let [[from to] (map parse-pattern (re-seq #"[.#/]+" line))]
              (reduce (fn [rules from*]
                        (assoc rules from* to))
                      rules
                      (symmetries from))))
          {}
          (str/split-lines s)))

(defn break [n pattern]
  (mapv (fn [y]
          (mapv (fn [x]
                  (mapv (fn [dy]
                          (mapv (fn [dx]
                                  (get-in pattern
                                          [(+ (* n y) dy) (+ (* n x) dx)]))
                                (range n)))
                        (range n)))
                (range (/ (count pattern) n))))
        (range (/ (count pattern) n))))

(defn reassemble [blocks]
  (let [block-size (count (get-in blocks [0 0]))]
    (mapv (fn [y]
            (mapv (fn [x]
                    (get-in blocks [(quot y block-size) (quot x block-size)
                                    (mod y block-size) (mod x block-size)]))
                  (range (* (count blocks) block-size))))
          (range (* (count blocks) block-size)))))

(defn part-* [rules]
  (iterate (fn [pattern]
             (->> pattern
                  (break (if (even? (count pattern)) 2 3))
                  (mapv #(mapv rules %))
                  reassemble))
           (parse-pattern ".#./..#/###")))

(defn part-1 []
  (->> "input/2017/21" slurp parse part-* (drop 5) first
       flatten (filter #{\#}) count))

(defn part-2 []
  (->> "input/2017/21" slurp parse part-* (drop 18) first
       flatten (filter #{\#}) count))

(deftest test-example
  (is (= (map parse-pattern [".#./..#/###"
                             "#..#/..../..../#..#"
                             "##.##./#..#../....../##.##./#..#../......"])
         (take 3 (part-* (parse "../.# => ##./#../...
                                 .#./..#/### => #..#/..../..../#..#"))))))
