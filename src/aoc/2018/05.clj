(ns aoc.2018.05
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn react [polymer]
  (reduce (fn [acc x]
            (let [y (first acc)]
              (if (and y (not= x y)
                       (= (Character/toLowerCase x) (Character/toLowerCase y)))
                (rest acc)
                (cons x acc))))
          nil
          polymer))

(defn part-1 []
  (->> "input/2018/05" slurp str/trim react count))

(defn part-2 []
  (let [polymer (->> "input/2018/05" slurp str/trim react)]
    (apply min (map #(count (react (remove #{(char (+ 65 %)) (char (+ 97 %))}
                                           polymer)))
                    (range 26)))))

(deftest test-example
  (is (= 10 (count (react "dabAcCaCBAcCcaDA")))))
