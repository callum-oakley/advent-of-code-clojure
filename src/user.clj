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
      (spit path
            (:body
             (client/get
              (format "https://adventofcode.com/%d/day/%d/input" year day)
              {:headers {"cookie" (str "session=" (slurp ".session"))}}))))))

(defn run-tests
  ([]
   (test/run-all-tests #"aoc\..*"))
  ([year]
   (if (<= year 25)
     (run-tests default-year year)
     (test/run-all-tests (re-pattern (format "aoc\\.%d\\..*" year)))))
  ([year day]
   (download-input year day)
   (test/run-all-tests (re-pattern (format "aoc\\.%d\\.%02d" year day)))))

(defn run
  ([]
   (run! run (range 2015 2021)))
  ([year]
   (if (<= year 25)
     (run default-year year)
     (run! #(run year %) (range 1 26))))
  ([year day]
   (if (<= year 25)
     (run default-year year day)
     (do
       (run year day 1)
       (run year day 2))))
  ([year day part]
   (let [sym (symbol (format "aoc.%d.%02d/part-%d" year day part))]
     (when-let [f (resolve sym)]
       (download-input year day)
       (let [start (System/currentTimeMillis)
             res (f)
             duration (double (/ (- (System/currentTimeMillis) start) 1000))]
         (println (format "%s %7.3fs   %s" sym duration res)))))))

(defn rrr [& args]
  (repl/refresh)
  (apply run-tests args)
  (apply run args))

(defn log []
  (spit "results.log" (with-out-str (run))))
