(ns aoc.2020.15
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map read-string (str/split s #",")))

;; Van Eck's sequence https://youtu.be/etMJxB-igrc
(defn turn [[prev i mem]]
  (let [seen (get mem prev)
        age (if seen (- i seen) 0)]
    [age (inc i) (assoc! mem prev i)]))

(defn game [n seed]
  (nth
   (->> (iterate
         turn
         [(last seed)
          (dec (count seed))
          (transient
           (apply assoc (vec (repeat n nil))
                  (flatten (map-indexed (fn [i m] [m i]) (drop-last seed)))))])
        rest
        (map first)
        (concat seed))
   (dec n)))

(defn part-1 [seed]
  (game 2020 seed))

(defn part-2 [seed]
  (game 30000000 seed))

(deftest test-examples
  (is (= (part-1 [0 3 6]) 436))
  (is (= (part-2 [0 3 6]) 175594)))
