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

(defn part-1 [seed]
  (part-* hash/md5 seed))

(defn part-2 [seed]
  (part-* stretched-md5 seed))

(deftest test-stretched-md5
  (is (= "a107ff634856bb300138cac6568c0f24"
         (hash/hex (stretched-md5 (.getBytes "abc0"))))))

(deftest test-part-*
  (is (= 22728 (part-* hash/md5 "abc"))))
