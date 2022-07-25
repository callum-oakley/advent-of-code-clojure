(ns aoc.2018.05
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn react [polymer]
  (reduce (fn [acc x]
            (let [y (first acc)]
              ;; uppercase and lowercase differ by 32
              (if (and y (= 32 (abs (- x y))))
                (rest acc)
                (cons x acc))))
          nil
          polymer))

(defn part-1 []
  (->> "input/2018/05" slurp str/trim (map int) react count))

(defn part-2 []
  (let [polymer (->> "input/2018/05" slurp str/trim (map int) react)]
    (apply min (map #(count (react (remove #{% (+ % 32)} polymer)))
                    (range 65 91)))))

(deftest test-example
  (is (= 10 (count (react (map int "dabAcCaCBAcCcaDA"))))))
