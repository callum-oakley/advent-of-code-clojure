(ns aoc.2021.12
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (reduce (fn [graph line]
            (let [[a b] (str/split (str/trim line) #"-")]
              (-> graph
                  (update a (fnil conj #{}) b)
                  (update b (fnil conj #{}) a))))
          {}
          (str/split-lines s)))

(defn paths [graph cave visited bonus?]
  (->> cave graph
       (keep #(cond
                (= % "start") nil
                (= % "end") 1
                (= % (str/upper-case %)) (paths graph % visited bonus?)
                (not (visited %)) (paths graph % (conj visited %) bonus?)
                bonus? (paths graph % visited false)))
       (apply +)))

(defn part-1* [graph]
  (paths graph "start" #{} false))

(defn part-2* [graph]
  (paths graph "start" #{} true))

(defn part-1 []
  (->> "input/2021/12" slurp parse part-1*))

(defn part-2 []
  (->> "input/2021/12" slurp parse part-2*))

(deftest test-examples
  (is (= 10 (part-1* (parse "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"))))
  (is (= 19 (part-1* (parse "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc
                             HN-end\nkj-sa\nkj-HN\nkj-dc"))))
  (is (= 226 (part-1* (parse "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg
                              zg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW
                              start-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"))))
  (is (= 36 (part-2* (parse "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"))))
  (is (= 103 (part-2* (parse "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc
                              HN-end\nkj-sa\nkj-HN\nkj-dc"))))
  (is (= 3509 (part-2* (parse "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg
                               zg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW
                               start-pj\nhe-WI\nzg-he\npj-fs\nstart-RW")))))
