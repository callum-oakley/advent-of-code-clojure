(ns aoc.2016.05
  (:require
   [aoc.hash :as hash]
   [clojure.test :refer [deftest is]]))

(defn part-1* [seed]
  (->> (iterate inc 0)
       (keep #(let [[a b c] (hash/md5 (.getBytes (str seed %)))]
                (when (= 0 a b (bit-and 0xf0 c))
                  (get (format "%02x" c) 1))))
       (take 8)
       (apply str)))

(defn part-2* [seed]
  (->> (iterate inc 0)
       (keep #(let [[a b c d] (hash/md5 (.getBytes (str seed %)))]
                (when (and (= 0 a b (bit-and 0xf0 c)) (<= (bit-and 0x0f c) 7))
                  [(bit-and 0x0f c) (get (format "%02x" d) 0)])))
       (reduce (fn [password [i c]]
                 (if (password i)
                   password
                   (let [password (assoc password i c)]
                     (if (some nil? password)
                       password
                       (reduced password)))))
               (vec (repeat 8 nil)))
       (apply str)))

(defn part-1 []
  (part-1* (slurp "input/2016/05")))

(defn part-2 []
  (part-2* (slurp "input/2016/05")))

(deftest test-part-1*
  (is (= "18f47a30" (part-1* "abc"))))

(deftest test-part-2*
  (is (= "05ace8e3" (part-2* "abc"))))

(deftest test-answers
  (is (= "f77a0e6e" (part-1)))
  (is (= "999828ec" (part-2))))
