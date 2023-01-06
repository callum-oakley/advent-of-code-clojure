(ns aoc.2015.04
  (:require
   [aoc.hash :as hash]
   [clojure.test :refer [deftest is]]))

(defn part-* [n key]
  (->> (iterate inc 1)
       (filter #(->> (str key %) .getBytes hash/md5 hash/nibbles (take n)
                     (every? zero?)))
       first))

(defn part-1 [s]
  (part-* 5 s))

(defn part-2 [s]
  (part-* 6 s))

(deftest test-part-1
  (is (= 609043 (part-1 "abcdef")))
  (is (= 1048970 (part-1 "pqrstuv"))))
