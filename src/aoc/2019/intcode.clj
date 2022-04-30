(ns aoc.2019.intcode
  (:refer-clojure :exclude [load]))

(defn load
  "Read a file as an Intcode program"
  [f]
  (->> f slurp (re-seq #"-?\d+") (mapv parse-long)))

(defn- run* [mem head base]
  (let [mode (vec (rest (reverse (str (mem head)))))
        ! #(case (get mode %)
             \1 (+ % head)
             \2 (+ base (mem (+ % head)))
             (mem (+ % head)))
        $ (comp mem !)]
    (case (mod (! 0) 100)
      1 (recur (assoc mem (! 3) (+ ($ 1) ($ 2))) (+ head 4) base)
      2 (recur (assoc mem (! 3) (* ($ 1) ($ 2))) (+ head 4) base)
      3 {:state :in :in (! 1) :mem mem :head head :base base}
      4 {:state :out :out ($ 1) :mem mem :head head :base base}
      5 (recur mem (if (zero? ($ 1)) (+ head 3) ($ 2)) base)
      6 (recur mem (if (zero? ($ 1)) ($ 2) (+ head 3)) base)
      7 (recur (assoc mem (! 3) (if (< ($ 1) ($ 2)) 1 0)) (+ head 4) base)
      8 (recur (assoc mem (! 3) (if (= ($ 1) ($ 2)) 1 0)) (+ head 4) base)
      9 (recur mem (+ head 2) (+ base ($ 1)))
      99 {:state :halt :mem mem})))

(defn run
  "Runs a vm on mem until a read, write, or halt operation is encountered, then
   returns the vm. Check :state and resume with >>, or wrap with io."
  [mem]
  (run* (vec (concat mem (repeat (max 0 (- 4096 (count mem))) 0))) 0 0))

(defn >>
  "Resumes a vm with any arguments given (none for :out and one for :in)."
  [vm & args]
  (case (:state vm)
    :in (run* (apply assoc (:mem vm) (:in vm) args) (+ (:head vm) 2) (:base vm))
    :out (run* (:mem vm) (+ (:head vm) 2) (:base vm))))

(defn io
  "Feeds input to the vm and collects output. Returns on halt or end of input,
   or when outn outputs have been collected."
  ([vm in] (io vm in ##Inf))
  ([vm in outn]
   (loop [vm vm in in out []]
     (case (:state vm)
       :in (if (seq in)
             (recur (>> vm (first in)) (rest in) out)
             [out vm])
       :out (if (< (inc (count out)) outn)
              (recur (>> vm) in (conj out (:out vm)))
              [(conj out (:out vm)) (>> vm)])
       :halt [out vm]))))

(defn run-io
  "Runs a vm on mem with the given input, returning output. Throws if input is
   exhausted before halt."
  [mem in]
  (let [[out vm] (io (run mem) in)]
    (case (:state vm)
      :input (throw (Exception. "Input exhausted"))
      :halt out)))

(defn run-interactive
  "Runs a vm on mem, taking input as ASCII from stdin, and writing output as
   ASCII on stdout. Reads and writes whole lines at a time."
  [mem]
  (loop [vm (run mem) in [] out []]
    (case (:state vm)
      :in (if (seq in)
            (recur (>> vm (first in)) (rest in) out)
            (recur vm (map int (str (read-line) "\n")) out))
      :out (if (= 10 (:out vm))
             (do
               (println (apply str (map char out)))
               (recur (>> vm) in []))
             (recur (>> vm) in (conj out (:out vm))))
      :halt nil)))
