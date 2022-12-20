(ns aoc.2022.20
  (:require
   [aoc.vector :refer [*v]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [vals (mapv read-string (re-seq #"-?\d+" s))
        m (count vals)]
    [vals (mapv #(mod (inc %) m) (range m)) (mapv #(mod (dec %) m) (range m))]))

(defn mix [[vals next prev]]
  (let [m (dec (count vals))]
    (reduce-kv (fn [[vals next prev] i val]
                 (let [p (prev i)
                       n (next i)
                       next (assoc next p n)
                       prev (assoc prev n p)
                       p (let [dist (mod val m)]
                           ((apply comp (if (< dist (/ m 2))
                                          (repeat dist next)
                                          (repeat (- m dist) prev)))
                            p))
                       n (next p)]
                   [vals (assoc next p i i n) (assoc prev i p n i)]))
               [vals next prev]
               vals)))

(defn part-1 [input]
  (let [[vals next _] (mix input)]
    (->> (iterate next 0) (map vals) (drop-while #(not (zero? %)))
         (take-nth 1000) rest (take 3) (apply +))))

(defn part-2 [[vals next prev]]
  (let [vals (*v 811589153 vals)
        [_ next _] ((apply comp (repeat 10 mix)) [vals next prev])]
    (->> (iterate next 0) (map vals) (drop-while #(not (zero? %)))
         (take-nth 1000) rest (take 3) (apply +))))

(deftest test-example
  (is (= 3 (part-1 (parse "1 2 -3 3 -2 0 4"))))
  (is (= 1623178306 (part-2 (parse "1 2 -3 3 -2 0 4")))))
