(ns aoc.2019.03
  (:require
   [aoc.vector :refer [+v manhattan-distance]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map (fn [line]
         (->> line (re-seq #"([UDLR])(\d+)") (map #(map read-string (rest %)))))
       (str/split-lines s)))

(defn expand [instructions]
  (mapcat (fn [[dir steps]] (repeat steps dir)) instructions))

(defn wire [instructions]
  (reductions (fn [pos dir]
                (+v pos (case dir U [-1 0] D [1 0] L [0 -1] R [0 1])))
              [0 0]
              (expand instructions)))

(defn map-intersection [f a b]
  (into {} (keep (fn [[k va]] (when-let [vb (b k)] [k (f va vb)])) a)))

(defn intersections [[a b]]
  (dissoc (map-intersection + (zipmap a (range)) (zipmap b (range))) [0 0]))

(defn part-1* [pair]
  (->> pair (map wire) intersections keys (map manhattan-distance) (apply min)))

(defn part-2* [pair]
  (->> pair (map wire) intersections vals (apply min)))

(defn part-1 []
  (->> "input/2019/03" slurp parse part-1*))

(defn part-2 []
  (->> "input/2019/03" slurp parse part-2*))

(deftest test-examples
  (is (= 6 (part-1* (parse "R8,U5,L5,D3\nU7,R6,D4,L4"))))
  (is (= 159 (part-1* (parse "R75,D30,R83,U83,L12,D49,R71,U7,L72
                              U62,R66,U55,R34,D71,R55,D58,R83"))))
  (is (= 135 (part-1* (parse "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
                              U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))))
  (is (= 30 (part-2* (parse "R8,U5,L5,D3\nU7,R6,D4,L4"))))
  (is (= 610 (part-2* (parse "R75,D30,R83,U83,L12,D49,R71,U7,L72
                              U62,R66,U55,R34,D71,R55,D58,R83"))))
  (is (= 410 (part-2* (parse "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
                              U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))
