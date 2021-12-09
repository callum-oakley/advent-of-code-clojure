(ns aoc.2017.22
  (:require
   [aoc.vector :refer [+v *v]]
   [aoc.grid :as grid]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (update-vals (grid/parse s) #(case % \. :clean \# :infected)))

(defn part-* [n transitions nodes]
  (->> {:pos [(/ (apply max (map first (keys nodes))) 2)
              (/ (apply max (map second (keys nodes))) 2)]
        :dir grid/north
        :nodes nodes
        :infections 0}
       (iterate (fn [{:keys [pos dir nodes infections]}]
                  (let [current (get nodes pos :clean)
                        dir (case current
                              :clean (grid/left dir)
                              :weakened dir
                              :infected (grid/right dir)
                              :flagged (*v -1 dir))
                        current (transitions current)]
                    {:pos (+v pos dir)
                     :dir dir
                     :nodes (assoc nodes pos current)
                     :infections (if (= :infected current)
                                   (inc infections)
                                   infections)})))
       (drop n) first :infections))

(def transitions-1
  {:clean :infected :infected :clean})

(def transitions-2
  {:clean :weakened :weakened :infected :infected :flagged :flagged :clean})

(defn part-1 []
  (->> "input/2017/22" slurp parse (part-* 10000 transitions-1)))

(defn part-2 []
  (->> "input/2017/22" slurp parse (part-* 10000000 transitions-2)))

(deftest test-examples
  (is (= 5 (part-* 7 transitions-1 (parse "..#\n#..\n..."))))
  (is (= 41 (part-* 70 transitions-1 (parse "..#\n#..\n..."))))
  (is (= 5587 (part-* 10000 transitions-1 (parse "..#\n#..\n..."))))
  (is (= 26 (part-* 100 transitions-2 (parse "..#\n#..\n..."))))
  (is (= 2511944 (part-* 10000000 transitions-2 (parse "..#\n#..\n...")))))
