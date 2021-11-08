(ns aoc.2020.15
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

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

(defn part-1 []
  (->> (str/split (slurp "input/2020/15") #",")
       (map read-string)
       (game 2020)))

(defn part-2 []
  (->> (str/split (slurp "input/2020/15") #",")
       (map read-string)
       (game 30000000)))

(deftest test-part-1
  (is (= (game 2020 [0 3 6]) 436))
  (is (= (part-1) 257)))

(deftest test-part-2
  (is (= (game 30000000 [0 3 6]) 175594))
  (is (= (part-2) 8546398)))
