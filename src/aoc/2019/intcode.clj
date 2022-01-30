(ns aoc.2019.intcode
  (:refer-clojure :exclude [load]))

(defn load
  "Read a file as an Intcode program"
  [f]
  (->> f slurp (re-seq #"-?\d+") (mapv parse-long)))

(defn >>
  "With only 1 argument, resumes a vm from :out state, otherwise resumes a vm
   from :in state passing the remaining arguments as input"
  ([vm] ((:k vm)))
  ([vm & input] (reduce (fn [vm in] ((:k vm) in)) vm input)))

(defn run
  "If passed only mem, runs mem until a read, write, or halt operation is
   encountered, then returns the vm. Check :state and resume with >>. If passed
   mem and input, runs mem on input, returning a lazy sequence of output."
  ([mem]
   ((fn go [mem head]
      (let [arg* #(mem (+ % head))
            mode (vec (reverse (str (arg* 0))))
            arg #(if (= \1 (get mode (inc %))) (arg* %) (mem (arg* %)))]
        (case (mod (arg* 0) 100)
          1 (recur (assoc mem (arg* 3) (+ (arg 1) (arg 2))) (+ head 4))
          2 (recur (assoc mem (arg* 3) (* (arg 1) (arg 2))) (+ head 4))
          3 {:state :in :k #(go (assoc mem (arg* 1) %) (+ head 2))}
          4 {:state :out :out (arg 1) :k #(go mem (+ head 2))}
          5 (recur mem (if (zero? (arg 1)) (+ head 3) (arg 2)))
          6 (recur mem (if (zero? (arg 1)) (arg 2) (+ head 3)))
          7 (recur (assoc mem (arg* 3) (if (< (arg 1) (arg 2)) 1 0)) (+ head 4))
          8 (recur (assoc mem (arg* 3) (if (= (arg 1) (arg 2)) 1 0)) (+ head 4))
          99 {:state :halt :mem mem})))
    mem 0))
  ([mem input]
   ((fn go [vm input]
      (case (:state vm)
        :in (if (seq input)
              (recur (>> vm (first input)) (rest input))
              (throw (Exception. "Input exhausted")))
        :out (cons (:out vm) (lazy-seq (go (>> vm) input)))
        :halt nil))
    (run mem) input)))
