(ns aoc.2015.04
  (:require
   [aoc.hash :as hash]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn part-* [n key]
  (->> (iterate inc 1)
       (filter #(->> (str key %) .getBytes hash/md5 hash/nibbles (take n)
                     (every? zero?)))
       first))

(defn part-1 []
  (part-* 5 (str/trim (slurp "input/2015/04"))))

(defn part-2 []
  (part-* 6 (str/trim (slurp "input/2015/04"))))

(deftest test-part-*
  (is (= 609043 (part-* 5 "abcdef")))
  (is (= 1048970 (part-* 5 "pqrstuv"))))
