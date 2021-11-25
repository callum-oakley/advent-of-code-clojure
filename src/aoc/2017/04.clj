(ns aoc.2017.04
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn valid-1? [passphrase]
  (let [words (str/split passphrase #" ")]
    (= (count words) (count (set words)))))

(defn valid-2? [passphrase]
  (let [words (map sort (str/split passphrase #" "))]
    (= (count words) (count (set words)))))

(defn part-1 []
  (->> "input/2017/04" slurp str/split-lines (filter valid-1?) count))

(defn part-2 []
  (->> "input/2017/04" slurp str/split-lines (filter valid-2?) count))

(deftest test-examples?
  (is (valid-1? "aa bb cc dd ee"))
  (is (not (valid-1? "aa bb cc dd aa")))
  (is (valid-1? "aa bb cc dd aaa"))
  (is (valid-2? "abcde fghij"))
  (is (not (valid-2? "abcde xyz ecdab")))
  (is (valid-2? "a ab abc abd abf abj"))
  (is (valid-2? "iiii oiii ooii oooi oooo"))
  (is (not (valid-2? "oiii ioii iioi iiio"))))
