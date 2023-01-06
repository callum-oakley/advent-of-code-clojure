(ns aoc.2015.20
  (:require
   [clojure.test :refer [deftest is]]))

(def parse read-string)

(defn deliver-presents [limit endurance multiplier]
  (persistent!
   (reduce (fn [presents elf]
             (transduce (comp (take-while #(< % limit)) (take endurance))
                        (completing
                         (fn [presents house]
                           (assoc! presents
                                   house
                                   (+ (get presents house 0)
                                      (* multiplier elf)))))
                        presents
                        (iterate #(+ elf %) elf)))
           (transient {})
           (range 1 limit))))

(defn part-* [limit endurance multiplier target]
  (->> (deliver-presents limit endurance multiplier)
       (filter (fn [[_ presents]] (<= target presents)))
       (map first)
       (apply min)))

(defn part-1 [target]
  (part-* 1000000 1000000 10 target))

(defn part-2 [target]
  (part-* 1000000 50 11 target))

(deftest test-deliver-presents
  (is (= {1 10 2 30 3 40 4 70 5 60 6 120 7 80 8 150 9 130}
         (deliver-presents 10 10 10))))
