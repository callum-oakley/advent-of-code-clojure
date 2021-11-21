(ns aoc.2015.11
  (:require
   [aoc.string :as aocstr]
   [clojure.test :refer [deftest is]]))

(defn incc [c]
  (-> c int inc char))

(defn valid? [password]
  (and (some (fn [[a b c]] (and (= b (incc a)) (= c (incc b))))
             (partition 3 1 password))
       (not-any? #{\i \o \l} password)
       (let [runs (->> (partition-by identity password)
                       (map count)
                       (filter #(<= 2 %)))]
         (or (<= 2 (count runs))
             (and (= 1 (count runs)) (<= 4 (first runs)))))))

(defn increment [password]
  (loop [password password
         i (dec (count password))]
    (let [c (get password i)]
      (if (= \z c)
        (recur (aocstr/assoc-at password i \a) (dec i))
        (aocstr/assoc-at password i (if (#{\h \n \k} c)
                                      (incc (incc c))
                                      (incc c)))))))

(defn next-valid [password]
  (->> password (iterate increment) rest (filter valid?) first))

(defn part-1 []
  (-> "input/2015/11" slurp next-valid))

(defn part-2 []
  (-> "input/2015/11" slurp next-valid next-valid))

(deftest test-valid?
  (is (not (valid? "hijklmmn")))
  (is (not (valid? "abbceffg")))
  (is (not (valid? "abbcegjk")))
  (is (valid? "abcdffaa"))
  (is (valid? "ghjaabcc")))

(deftest test-next-valid
  (is (= "abcdffaa" (next-valid "abcdefgh")))
  (is (= "ghjaabcc" (next-valid "ghijklmn"))))

(deftest test-answers
  (is (= "vzbxxyzz" (part-1)))
  (is (= "vzcaabcc" (part-2))))
