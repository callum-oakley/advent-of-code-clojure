(ns aoc.2016.14
  (:require
   [aoc.hash :as hash]
   [clojure.test :refer [deftest is]]))

(def stretched-md5
  (apply comp (interpose hash/hex-bytes (repeat 2017 hash/md5))))

(defn triple [s]
  (some #(when (>= (count %) 3) (first %)) (partition-by identity s)))

(defn part-* [hash seed]
  (nth (->> (range)
            (map (fn [i] [i (->> (str seed i) .getBytes hash hash/hex)]))
            (partition 1001 1)
            (keep (fn [[[i h] & next-1000]]
                    (when-let [c (triple h)]
                      (some #(when (re-find (re-pattern (str c "{5}")) %) i)
                            (map second next-1000))))))
       63))

(defn part-1 []
  (part-* hash/md5 (slurp "input/2016/14")))

(defn part-2 []
  (part-* stretched-md5 (slurp "input/2016/14")))

(deftest test-stretched-md5
  (is (= "a107ff634856bb300138cac6568c0f24"
         (hash/hex (stretched-md5 (.getBytes "abc0"))))))

(deftest test-part-*
  (is (= 22728 (part-* hash/md5 "abc"))))

(deftest test-answers
  (is (= 15168 (part-1)))
  (is (= 20864 (part-2))))
