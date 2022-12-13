(ns aoc.2022.09
  (:require
   [aoc.vector :refer [+v -v chessboard-distance]]
   [aoc.grid :refer [north east south west]]
   [clojure.math :as math]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\w+") (map read-string) (partition 2)))

(defn follow [tail head]
  (if (<= (chessboard-distance tail head) 1)
    tail
    (+v tail (mapv #(int (math/signum %)) (-v head tail)))))

(defn move [rope dir]
  (reduce (fn [rope i] (update rope i follow (rope (dec i))))
          (update rope 0 +v dir)
          (range 1 (count rope))))

(defn part-* [length instructions]
  (count
   (first
    (reduce (fn [[visited rope] [dir n]]
              (if (zero? n)
                [visited rope]
                (let [rope (move rope (case dir U north R east D south L west))]
                  (recur [(conj visited (peek rope)) rope] [dir (dec n)]))))
            [#{} (vec (repeat length [0 0]))]
            instructions))))

(defn part-1 [data]
  (part-* 2 data))

(defn part-2 [data]
  (part-* 10 data))

(deftest test-example
  (is (= 13 (part-1 (parse "R 4 U 4 L 3 D 1 R 4 D 1 L 5 R 2"))))
  (is (= 1 (part-2 (parse "R 4 U 4 L 3 D 1 R 4 D 1 L 5 R 2"))))
  (is (= 36 (part-2 (parse "R 5 U 8 L 8 D 3 R 17 D 10 L 25 U 20")))))
