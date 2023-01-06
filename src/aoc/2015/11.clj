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

(defn part-1 [password]
  (->> password (iterate increment) rest (filter valid?) first))

(defn part-2 [password]
  (part-1 (part-1 password)))

(deftest test-examples
  (is (not (valid? "hijklmmn")))
  (is (not (valid? "abbceffg")))
  (is (not (valid? "abbcegjk")))
  (is (valid? "abcdffaa"))
  (is (valid? "ghjaabcc"))
  (is (= "abcdffaa" (part-1 "abcdefgh")))
  (is (= "ghjaabcc" (part-1 "ghijklmn"))))
