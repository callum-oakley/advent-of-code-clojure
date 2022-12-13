(ns aoc.2022.07
  (:require
   [aoc.search :as search]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> (str/split-lines s)
       (map #(mapv read-string (re-seq #"\S+" %)))
       (reduce
        (fn [[fs path] line]
          (cond
            (= '[$ cd ..] line) [fs (pop path)]
            (= '[$ cd] (take 2 line)) [fs (conj path (line 2))]
            (= '[$ ls] line) [fs path]
            (= 'dir (line 0)) [(update-in fs path assoc (line 1) {}) path]
            :else [(update-in fs path assoc (line 1) (line 0)) path]))
        [{} []])
       first))

(defn size [fs]
  (if (int? fs) fs (apply + (map size (vals fs)))))

(defn part-* [fs]
  (map size (search/bft fs #(filter map? (vals %)) identity)))

(defn part-1 [fs]
  (apply + (filter #(<= % 100000) (part-* fs))))

(defn part-2 [fs]
  (let [free (- 70000000 (size fs))]
    (apply min (filter #(<= 30000000 (+ free %)) (part-* fs)))))

(def example
  "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls
   dir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..
   $ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k")

(deftest test-example
  (is (= 95437 (part-1 (parse example))))
  (is (= 24933642 (part-2 (parse example)))))
