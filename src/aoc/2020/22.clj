(ns aoc.2020.22
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map #(->> % rest (mapv read-string))
       (map str/split-lines (str/split s #"\n\n"))))

(defn score [deck]
  (apply + (map-indexed (fn [i c] (* c (inc i))) (reverse deck))))

(defn game-1 [[d1 d2]]
  (cond
    (empty? d1) {:winner 2 :deck d2}
    (empty? d2) {:winner 1 :deck d1}
    :else (let [c1 (first d1) c2 (first d2)
                d1* (subvec d1 1) d2* (subvec d2 1)]
            (if (> c1 c2)
              (recur [(conj d1* c1 c2) d2*])
              (recur [d1* (conj d2* c2 c1)])))))

(defn game-2 [[d1 d2] seen]
  (cond
    (empty? d1) {:winner 2 :deck d2}
    (empty? d2) {:winner 1 :deck d1}
    (seen [d1 d2]) {:winner 1 :deck d1}
    :else (let [c1 (first d1) c2 (first d2)
                d1* (subvec d1 1) d2* (subvec d2 1)
                winner (if (and (<= c1 (count d1*)) (<= c2 (count d2*)))
                         (:winner
                          (game-2 [(subvec d1* 0 c1) (subvec d2* 0 c2)] #{}))
                         (if (> c1 c2) 1 2))
                seen* (conj seen [d1 d2])]
            (case winner
              1 (recur [(conj d1* c1 c2) d2*] seen*)
              2 (recur [d1* (conj d2* c2 c1)] seen*)))))

(defn part-1 [[d1 d2]]
  (score (:deck (game-1 [d1 d2]))))

(defn part-2 [[d1 d2]]
  (score (:deck (game-2 [d1 d2] #{}))))

(def sample
  "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10")

(deftest test-examples
  (is (= (part-1 (parse sample)) 306))
  (is (= (part-2 (parse sample)) 291)))
