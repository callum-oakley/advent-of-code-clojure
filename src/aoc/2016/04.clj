(ns aoc.2016.04
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [[_ name id checksum] (re-matches #"([a-z-]+)-(\d+)\[([a-z]{5})\]" s)]
    {:name name
     :id (read-string id)
     :checksum checksum}))

(defn checksum [name]
  (->> name (remove #{\-}) frequencies (sort-by key) (sort-by val >)
       (map key) (take 5) (apply str)))

(defn real? [room]
  (= (:checksum room) (checksum (:name room))))

(defn shift [n c]
  (case c
    \- \space
    (char (+ (mod (+ n (- (int c) (int \a))) 26) (int \a)))))

(defn decrypt [name key]
  (apply str (map #(shift key %) name)))

(defn part-1 []
  (->> "input/2016/04" slurp str/split-lines (map parse) (filter real?)
       (map :id) (apply +)))

(defn part-2 []
  (->> "input/2016/04" slurp str/split-lines (map parse) (filter real?)
       (filter #(re-find #"north" (decrypt (:name %) (:id %))))
       first :id))

(deftest test-real?
  (is (real? (parse "aaaaa-bbb-z-y-x-123[abxyz]")))
  (is (real? (parse "a-b-c-d-e-f-g-h-987[abcde]")))
  (is (real? (parse "not-a-real-room-404[oarel]")))
  (is (not (real? (parse "totally-real-room-200[decoy]")))))

(deftest test-decrypt
  (is (= "very encrypted name" (decrypt "qzmt-zixmtkozy-ivhz" 343))))
