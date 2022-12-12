(ns aoc.2016.17
  (:require
   [aoc.hash :as hash]
   [aoc.search :as search]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn adjacent [seed {[x y] :pos path :path}]
  (when (not= [3 3] [x y])
    (let [[a b c d] (hash/nibbles (hash/md5 (.getBytes (str seed path))))]
      (cond-> nil
        (and (pos? y) (< 10 a)) (conj {:pos [x (dec y)] :path (str path \U)})
        (and (< y 3) (< 10 b)) (conj {:pos [x (inc y)] :path (str path \D)})
        (and (pos? x) (< 10 c)) (conj {:pos [(dec x) y] :path (str path \L)})
        (and (< x 3) (< 10 d)) (conj {:pos [(inc x) y] :path (str path \R)})))))

(defn part-1* [seed]
  (:path (search/bfs {:pos [0 0] :path ""}
                     #(adjacent seed %)
                     identity
                     #(= [3 3] (:pos %)))))

(defn part-2* [seed]
  (->> (search/dft {:pos [0 0] :path ""} #(adjacent seed %) identity)
       (filter #(= [3 3] (:pos %)))
       (map #(count (:path %)))
       (apply max)))

(defn part-1 []
  (part-1* (str/trim (slurp "input/2016/17"))))

(defn part-2 []
  (part-2* (str/trim (slurp "input/2016/17"))))

(deftest test-part-1*
  (is (= "DDRRRD" (part-1* "ihgpwlah")))
  (is (= "DDUDRLRRUDRD" (part-1* "kglvqrro")))
  (is (= "DRURDRUDDLLDLUURRDULRLDUUDDDRR" (part-1* "ulqzkmiv"))))

(deftest test-part-2*
  (is (= 370 (part-2* "ihgpwlah")))
  (is (= 492 (part-2* "kglvqrro")))
  (is (= 830 (part-2* "ulqzkmiv"))))
