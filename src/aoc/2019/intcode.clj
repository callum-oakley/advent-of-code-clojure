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
   ((fn go [mem head base]
      (let [mode (vec (rest (reverse (str (mem head)))))
            ! #(case (get mode %)
                 \1 (+ % head)
                 \2 (+ base (mem (+ % head)))
                 (mem (+ % head)))
            $ (comp mem !)]
        (case (mod (! 0) 100)
          1 (recur (assoc mem (! 3) (+ ($ 1) ($ 2))) (+ head 4) base)
          2 (recur (assoc mem (! 3) (* ($ 1) ($ 2))) (+ head 4) base)
          3 {:state :in :k #(go (assoc mem (! 1) %) (+ head 2) base)}
          4 {:state :out :out ($ 1) :k #(go mem (+ head 2) base)}
          5 (recur mem (if (zero? ($ 1)) (+ head 3) ($ 2)) base)
          6 (recur mem (if (zero? ($ 1)) ($ 2) (+ head 3)) base)
          7 (recur (assoc mem (! 3) (if (< ($ 1) ($ 2)) 1 0)) (+ head 4) base)
          8 (recur (assoc mem (! 3) (if (= ($ 1) ($ 2)) 1 0)) (+ head 4) base)
          9 (recur mem (+ head 2) (+ base ($ 1)))
          99 {:state :halt :mem mem})))
    (vec (concat mem (repeat (max 0 (- 2048 (count mem))) 0))) 0 0))
  ([mem input]
   ((fn go [vm input]
      (case (:state vm)
        :in (if (seq input)
              (recur (>> vm (first input)) (rest input))
              (throw (Exception. "Input exhausted")))
        :out (cons (:out vm) (lazy-seq (go (>> vm) input)))
        :halt nil))
    (run mem) input)))
