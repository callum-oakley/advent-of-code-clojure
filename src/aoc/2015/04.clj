(ns aoc.2015.04
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]])
  (:import java.security.MessageDigest))

(defn md5 [bs]
  (.digest (MessageDigest/getInstance "MD5") bs))

(defn part-1* [key]
  (->> (iterate inc 1)
       ;; No need to construct the hex string, just check the first few bytes
       (filter #(let [[a b c] (md5 (.getBytes (str key %)))]
                  (= 0 a b (bit-and 0xf0 c))))
       first))

(defn part-2* [key]
  (->> (iterate inc 1)
       (filter #(= [0 0 0] (take 3 (md5 (.getBytes (str key %))))))
       first))

(defn part-1 []
  (part-1* (slurp "input/2015/04")))

(defn part-2 []
  (part-2* (slurp "input/2015/04")))

(deftest test-part-1*
  (is (= 609043 (part-1* "abcdef")))
  (is (= 1048970 (part-1* "pqrstuv"))))

(deftest test-answers
  (is (= 117946 (part-1)))
  (is (= 3938038 (part-2))))
