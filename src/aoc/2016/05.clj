(ns aoc.2016.05
  (:require
   [aoc.hash :as hash]
   [clojure.test :refer [deftest is]]))

(defn part-1* [seed]
  (->> (range)
       (keep #(let [[a b c d e f]
                    (hash/nibbles (hash/md5 (.getBytes (str seed %))))]
                (when (= 0 a b c d e)
                  (char (hash/hex-ch f)))))
       (take 8)
       (apply str)))

(defn part-2* [seed]
  (->> (range)
       (keep #(let [[a b c d e f g]
                    (hash/nibbles (hash/md5 (.getBytes (str seed %))))]
                (when (and (= 0 a b c d e) (<= f 7))
                  [f (char (hash/hex-ch g))])))
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
