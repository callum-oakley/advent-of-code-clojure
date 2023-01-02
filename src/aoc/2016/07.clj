(ns aoc.2016.07
  (:require
   [aoc.vector :refer [transpose]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse-ip [s]
  (->> s (re-seq #"([a-z]+)\[?([a-z]*)\]?") (map rest) transpose))

(defn parse [s]
  (map parse-ip (str/split-lines s)))

(defn abba? [s]
  (some (fn [[a b c d]] (and (= a d) (= b c) (not= a b))) (partition 4 1 s)))

(defn abas [s]
  (filter (fn [[a b c]] (and (= a c) (not= a b))) (partition 3 1 s)))

(defn tls? [[super hyper]]
  (and (some abba? super) (not (some abba? hyper))))

(defn ssl? [[super hyper]]
  (some (fn [[a b]] (some #(re-find (re-pattern (str b a b)) %) hyper))
        (mapcat abas super)))

(defn part-1 [ips]
  (->> ips (filter tls?) count))

(defn part-2 [ips]
  (->> ips (filter ssl?) count))

(deftest test-tls?
  (is (tls? (parse-ip "abba[mnop]qrst")))
  (is (not (tls? (parse-ip "abcd[bddb]xyyx"))))
  (is (not (tls? (parse-ip "aaaa[qwer]tyui"))))
  (is (tls? (parse-ip "ioxxoj[asdfgh]zxcvbn"))))

(deftest test-ssl?
  (is (ssl? (parse-ip "aba[bab]xyz")))
  (is (not (ssl? (parse-ip "xyx[xyx]xyx"))))
  (is (ssl? (parse-ip "aaa[kek]eke")))
  (is (ssl? (parse-ip "zazbz[bzb]cdb"))))
