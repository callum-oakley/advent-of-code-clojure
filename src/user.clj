(ns user
  (:require
   [clojure.test :as test]
   [clojure.java.io :as io]
   [clojure.tools.namespace.repl :as repl]
   [clojure.tools.namespace.find :as find]
   [clj-http.client :as client]))

(def default-year 2017)

(run! require (find/find-namespaces-in-dir (io/file "src")))

(defn download-input [year day]
  (let [path (format "input/%d/%02d" year day)]
    (when-not (.exists (io/file path))
      (io/make-parents path)
      (spit path
            (:body
             (client/get
              (format "https://adventofcode.com/%d/day/%d/input" year day)
              {:headers {:cookie (str "session=" (slurp ".session"))}}))))))

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
  (if-let [target (get-answer year day part)]
    (if (= target (str answer)) "OK" "WRONG")
    (let [res (submit-answer year day part answer)]
      (cond
        (re-find #"That's the right answer" res) "OK"
        (re-find #"That's not the right answer" res) "WRONG"
        (re-find #"You gave an answer too recently" res) "WAIT"
        :else (throw (ex-info "Unexpected response" {:res res}))))))

(defn run-tests
  ([]
   (test/run-all-tests #"aoc\..*"))
  ([year]
   (if (<= year 25)
     (run-tests default-year year)
     (test/run-all-tests (re-pattern (format "aoc\\.%d\\..*" year)))))
  ([year day]
   (test/run-all-tests (re-pattern (format "aoc\\.%d\\.%02d" year day)))))

(defn run-scrap [year day]
  (when-let [f (resolve (symbol (format "aoc.%d.%02d/scrap" year day)))]
    (download-input year day)
    (println (f))))

(defn run
  ([]
   (run! run (range 2015 (inc (.getValue (java.time.Year/now))))))
  ([year]
   (if (<= year 25)
     (run default-year year)
     (run! #(run year %) (range 1 26))))
  ([year day]
   (if (<= year 25)
     (run default-year year day)
     (do
       (run-scrap year day)
       (run year day 1)
       (run year day 2))))
  ([year day part]
   (let [sym (symbol (format "aoc.%d.%02d/part-%d" year day part))]
     (when-let [f (resolve sym)]
       (download-input year day)
       (let [start (System/currentTimeMillis)
             answer (f)
             duration (double (/ (- (System/currentTimeMillis) start) 1000))
             check (check-answer year day part answer)]
         (println (format "%s %7.3fs   %-44s %s" sym duration answer check)))))))

(defn rrr [& args]
  (repl/refresh)
  (apply run-tests args)
  (apply run args))

(defn log []
  (spit "results.log" (with-out-str (run))))
