(ns user
  (:require
   [clj-http.client :as client]
   [clojure.java.io :as io]
   [clojure.test :as test]
   [clojure.tools.namespace.find :as find]
   [clojure.tools.namespace.repl :as repl]))

(def default-year 2022)

(run! require (find/find-namespaces-in-dir (io/file "src")))

(defn download-input [year day]
  (let [path (format "input/%d/%02d" year day)]
    (when-not (.exists (io/file path))
      (io/make-parents path)
      (spit path
            (:body
             (client/get
              (format "https://adventofcode.com/%d/day/%d/input" year day)
              {:headers {:cookie (str "session=" (slurp ".session"))}}))))
    (slurp path)))

(defn get-answer* [year day part]
  (nth (map
        second
        (re-seq #"Your puzzle answer was <code>([^<]+)</code>"
                (:body
                 (client/get
                  (format "https://adventofcode.com/%d/day/%d" year day)
                  {:headers {:cookie (str "session=" (slurp ".session"))}}))))
       (dec part)
       nil))

(defn get-answer [year day part]
  (let [path (format "answers/%d/%02d/%d" year day part)]
    (if (.exists (io/file path))
      (slurp path)
      (when-let [answer (get-answer* year day part)]
        (io/make-parents path)
        (spit path answer)
        answer))))

(defn submit-answer [year day part answer]
  (:body
   (client/post
    (format "https://adventofcode.com/%d/day/%d/answer" year day)
    {:headers {:cookie (str "session=" (slurp ".session"))}
     :form-params {:level part :answer (str answer)}})))

(defn check-answer [year day part answer]
  (if (or (int? answer) (string? answer))
    (if-let [target (get-answer year day part)]
      (if (= target (str answer)) "*" "WRONG")
      (let [res (submit-answer year day part answer)]
        (cond
          (re-find #"That's the right answer" res) (do 
                                                     (get-answer year day part)
                                                     "*")
          (re-find #"That's not the right answer" res) "WRONG"
          (re-find #"You gave an answer too recently" res) "WAIT"
          :else (throw (ex-info "Unexpected response" {:res res})))))
    "WEIRD"))

(defn run-tests
  ([]
   (test/run-all-tests #"aoc\..*"))
  ([year]
   (if (<= year 25)
     (run-tests default-year year)
     (test/run-all-tests (re-pattern (format "aoc\\.%d\\..*" year)))))
  ([year day]
   (test/run-all-tests (re-pattern (format "aoc\\.%d\\.%02d" year day)))))

(defn with-timer [f]
  (let [start (System/currentTimeMillis)
        res (f)]
    [res (double (/ (- (System/currentTimeMillis) start) 1000))]))

(defn run
  ([]
   (let [[_ duration] (with-timer (fn [] (run! run (range 2015 2023))))]
     (println (format "aoc      (total) %9.3fs" duration))))
  ([year]
   (if (<= year 25)
     (run default-year year)
     (let [[_ duration] (with-timer (fn [] (run! #(run year %) (range 1 26))))]
       (println (format "aoc.%d (total) %9.3fs" year duration)))))
  ([year day]
   (if (<= year 25)
     (run default-year year day)
     (run! #(run year day %) [1 2])))
  ([year day part]
   (let [sym (symbol (format "aoc.%d.%02d/part-%d" year day part))]
     (when-let [solve (resolve sym)]
       (let [parse (resolve (symbol (format "aoc.%d.%02d/parse" year day)))
             input ((or parse identity) (download-input year day))
             [answer duration] (with-timer #(solve input))
             check (check-answer year day part answer)]
         (println
          (format "%s %7.3fs   %-44s %s" sym duration answer check)))))))

(defn rrr [& args]
  (repl/refresh)
  (let [{:keys [fail error]} (apply run-tests args)]
    (when (and (zero? fail) (zero? error))
      (apply run args))))

(defn log []
  (spit "results.log" (with-out-str (run))))
