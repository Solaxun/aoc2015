(ns aoc-2015.aoc12
  (require [clojure.java.io :as io]
           [clojure.set :refer :all]
           [clojure.string :as str]
           [clojure.repl :refer [source]]
           [clojure.math.combinatorics :as combo]
           [clojure.data.json :as json]))

(def data (slurp "src/aoc_2015/aoc12.txt"))

(defn has-red? [data]
  (some #{"red"}
        (concat (keys data) (vals data))))

(defn parse-json [json]
  (let [fst (first json)
        rst (rest json)]
    (cond
     (empty? json) nil
     (and (map? json) (has-red? json)) nil
     (map? fst) (if (has-red? fst)
                  (parse-json rst)
                  (concat (parse-json fst)
                          (parse-json rst)))
     (vector? fst)  (concat (parse-json fst)
                            (parse-json rst))
     (number? fst) (cons fst (parse-json rst))
     :else (parse-json rst))))

(defn cheating-parse-json [json]
  (->> json
       str
       (re-seq  #"-*\d\.*\d*")
       (map read-string)
       (reduce +)))

(reduce + (parse-json (json/read-str data)))
(cheating-parse-json (json/read-str data))
