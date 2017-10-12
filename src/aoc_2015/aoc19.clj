(ns aoc-2015.aoc19
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]))

(def data (str/split-lines (slurp "src/aoc_2015/aoc19.txt")))
(def molecule (last data))
(def replacements (drop-last 2 data))

(defn parse-rules [rules]
  (map (fn [rule] (str/split rule #" => ")) rules))

(defn index-of-occurences [word target]
  (set (remove nil?
               (map-indexed (fn [i x] (clojure.string/index-of word target i))
                            word))))

(defn all-replacements [word target replacement]
  (let [all-indexes (index-of-occurences word target)]
    (map (fn [i]  (str (subs word 0 i)
                      replacement
                      (subs word (+ i (count target)))))
         all-indexes)))

(defn process-molecule [molecule rulemap]
  (set (flatten (for [[old new] rulemap]
                  (all-replacements molecule old new)))))

(count (process-molecule molecule (parse-rules replacements)))
