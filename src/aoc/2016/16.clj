(ns aoc.2016.16
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn grow [data]
  (into (conj data \0) (map {\0 \1 \1 \0}) (rseq data)))

(defn checksum [data]
  (if (even? (count data))
    (recur (mapv (fn [[a b]] (if (= a b) \1 \0)) (partition 2 data)))
    data))

(defn part-* [disk seed]
  (apply str
         (checksum
          (subvec (first (filter #(<= disk (count %))
                                 (iterate grow (vec seed))))
                  0
                  disk))))

(defn part-1 []
  (part-* 272 (str/trim (slurp "input/2016/16"))))

(defn part-2 []
  (part-* 35651584 (str/trim (slurp "input/2016/16"))))

(deftest test-grow
  (is (= (vec "100") (grow (vec "1"))))
  (is (= (vec "001") (grow (vec "0"))))
  (is (= (vec "11111000000") (grow (vec "11111"))))
  (is (= (vec "1111000010100101011110000") (grow (vec "111100001010")))))

(deftest test-checksum
  (is (= (vec "100") (checksum (vec "110010110100")))))

(deftest test-part-*
  (is (= "01100" (part-* 20 "10000"))))
