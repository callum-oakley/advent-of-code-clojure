(ns aoc.2018.22
  (:require
   [aoc.grid :as grid]
   [aoc.search :as search]
   [aoc.vector :refer [manhattan-distance]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [[depth x y] (map read-string (re-seq #"\d+" s))]
    [depth [y x]]))

(def erosion
  (memoize
   (fn [depth target [y x]]
     (mod (+ (cond
               (or (= [0 0] [y x]) (= target [y x])) 0
               (zero? y) (* x 16807)
               (zero? x) (* y 48271)
               :else (* (erosion depth target [(dec y) x])
                        (erosion depth target [y (dec x)])))
             depth)
          20183))))

(defn part-1 [[depth [ty tx]]]
  (->> (for [y (range (inc ty)) x (range (inc tx))] [y x])
       (map #(mod (erosion depth [ty tx] %) 3))
       (apply +)))

(defn part-2 [[depth target]]
  (let [ok? (fn [tool pos]
              (not (#{[:neither :rocky] [:torch :wet] [:climbing-gear :narrow]}
                    [tool (case (mod (erosion depth target pos) 3)
                            0 :rocky 1 :wet 2 :narrow)])))]
    (:mins
     (search/a* {:pos [0 0] :tool :torch :mins 0}
                (fn [{:keys [pos tool mins]}]
                  (concat (keep #(when (and (not (some neg? %)) (ok? tool %))
                                   {:pos % :tool tool :mins (inc mins)})
                                (grid/adjacent pos))
                          (keep #(when (ok? % pos)
                                   {:pos pos :tool % :mins (+ mins 7)})
                                (disj #{:neither :torch :climbing-gear} tool))))
                #(dissoc % :mins)
                #(and (= (:pos %) target) (= (:tool %) :torch))
                :mins
                #(manhattan-distance (:pos %) target)))))

(deftest test-examples
  (is (= 1805 (erosion 510 [10 10] [1 1])))
  (is (= 114 (part-1 [510 [10 10]])))
  (is (= 45 (part-2 [510 [10 10]]))))
