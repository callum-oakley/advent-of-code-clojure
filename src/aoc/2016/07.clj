(ns aoc.2016.07
  (:require
   [aoc.vector :refer [transpose]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"([a-z]+)\[?([a-z]*)\]?") (map rest) transpose))

(defn abba? [s]
  (some (fn [[a b c d]] (and (= a d) (= b c) (not= a b))) (partition 4 1 s)))

(defn abas [s]
  (filter (fn [[a b c]] (and (= a c) (not= a b))) (partition 3 1 s)))

(defn tls? [[super hyper]]
  (and (some abba? super) (not (some abba? hyper))))

(defn ssl? [[super hyper]]
  (some (fn [[a b]] (some #(re-find (re-pattern (str b a b)) %) hyper))
        (mapcat abas super)))

(defn part-1 []
  (->> "input/2016/07" slurp str/split-lines (map parse) (filter tls?) count))

(defn part-2 []
  (->> "input/2016/07" slurp str/split-lines (map parse) (filter ssl?) count))

(deftest test-tls?
  (is (tls? (parse "abba[mnop]qrst")))
  (is (not (tls? (parse "abcd[bddb]xyyx"))))
  (is (not (tls? (parse "aaaa[qwer]tyui"))))
  (is (tls? (parse "ioxxoj[asdfgh]zxcvbn"))))

(deftest test-ssl?
  (is (ssl? (parse "aba[bab]xyz")))
  (is (not (ssl? (parse "xyx[xyx]xyx"))))
  (is (ssl? (parse "aaa[kek]eke")))
  (is (ssl? (parse "zazbz[bzb]cdb"))))
