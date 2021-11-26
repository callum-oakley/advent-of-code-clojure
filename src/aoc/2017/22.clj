(ns aoc.2017.22
  (:require
   [aoc.vectors :refer [+v *v]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(def left
  {[0 1] [-1 0] [-1 0] [0 -1] [0 -1] [1 0] [1 0] [0 1]})

(def right
  (set/map-invert left))

(defn parse [s]
  (first
   (reduce (fn [[nodes y] line]
             [(first
               (reduce (fn [[nodes x] c]
                         [(assoc nodes [y x] (case c \. :clean \# :infected))
                          (inc x)])
                       [nodes 0]
                       line))
              (inc y)])
           [{} 0]
           (str/split-lines s))))

(defn part-* [n transitions nodes]
  (:infections
   (nth (iterate (fn [{:keys [pos dir nodes infections]}]
                   (let [current (get nodes pos :clean)
                         dir (case current
                                :clean (left dir)
                                :weakened dir
                                :infected (right dir)
                                :flagged (*v -1 dir))
                         current (transitions current)]
                     {:pos (+v pos dir)
                      :dir dir
                      :nodes (assoc nodes pos current)
                      :infections (if (= :infected current)
                                    (inc infections)
                                    infections)}))
                 {:pos [(/ (apply max (map first (keys nodes))) 2)
                        (/ (apply max (map second (keys nodes))) 2)]
                  :dir [-1 0]
                  :nodes nodes
                  :infections 0})
        n)))

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
