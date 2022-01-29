(ns aoc.2019.intcode
  (:refer-clojure :exclude [load]))

(defn load [f]
  (->> f slurp (re-seq #"-?\d+") (mapv parse-long)))

(defn run
  ([mem] (run mem 0))
  ([mem head]
   (let [arg #(mem (+ % head))
         mode (vec (reverse (str (arg 0))))
         arg* #(if (= \1 (get mode (inc %))) (arg %) (mem (arg %)))]
     (case (mod (arg 0) 100)
       1 (recur (assoc mem (arg 3) (+ (arg* 1) (arg* 2))) (+ head 4))
       2 (recur (assoc mem (arg 3) (* (arg* 1) (arg* 2))) (+ head 4))
       3 {:state :in :k #(run (assoc mem (arg 1) %) (+ head 2))}
       4 {:state :out :out (arg* 1) :k #(run mem (+ head 2))}
       5 (recur mem (if (zero? (arg* 1)) (+ head 3) (arg* 2)))
       6 (recur mem (if (zero? (arg* 1)) (arg* 2) (+ head 3)))
       7 (recur (assoc mem (arg 3) (if (< (arg* 1) (arg* 2)) 1 0)) (+ head 4))
       8 (recur (assoc mem (arg 3) (if (= (arg* 1) (arg* 2)) 1 0)) (+ head 4))
       99 {:state :halt :mem mem}))))

(defn run-io [mem input]
  ((fn go [{:keys [state out k]} input]
     (case state
       :in (recur (k (first input)) (rest input))
       :out (cons out (lazy-seq (go (k) input)))
       :halt nil))
   (run mem) input))
