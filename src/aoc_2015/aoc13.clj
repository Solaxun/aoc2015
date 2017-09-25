(ns aoc-2015.aoc13
  (require [clojure.java.io :as io]
           [clojure.set :refer :all]
           [clojure.string :as str]
           [clojure.repl :refer [source]]
           [clojure.math.combinatorics :as combo]
           [clojure.data.json :as json]))

(def data (str/split (slurp "src/aoc_2015/aoc13.txt") #"\n"))

(defn get-people [text]
  ((juxt first (comp #(apply str %) drop-last last)) (str/split text #" ")))

(defn prefs [text]
  (let [[p1 _ gl pts _ _ _ _ _ _ p2] (str/split text #" ")
        pts (read-string pts)
        p2 (apply str (drop-last p2))
        pts (if (= gl "gain") pts (- pts))]
    [[p1 p2] pts]))

(defn people [data]
  (->> data
       (map get-people)
       flatten
       (apply hash-set)))

(defn arrangements [people]
  (combo/permutations people))

(defn score-arrangement [arrangement]
  (let [pairings (conj (partition 2 1 arrangement)
                       ((juxt first last) arrangement))
        rev-pairings (map reverse pairings)]
    (reduce + (map #_scores scores2 (concat pairings rev-pairings))))) ;use "scores" for part 1

(def scores (into {} (map prefs data)))
(def scores2 (merge scores my-scores))

;don't conj "Mark" onto people for part 1
(apply max (map score-arrangement (arrangements (conj (people data) "Mark"))))

(def my-scores 
  (let [pairings (map #(vector %1 "Mark") (people data))
        rev-pairings (map reverse pairings)
        all-pairings (concat pairings rev-pairings)]
    (into {} (map #(vector % 0) all-pairings))))





((fn [s1 s2] (into #{} (map (fn [x] (map (fn [y] [x y]) s2)) s1)))
 #{:a :b :c} #{1 2 3})
