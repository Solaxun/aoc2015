(ns aoc-2015.aoc19
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.set :refer [intersection]]
            [clojure.repl :refer [source]]))

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

(defn process-molecule
  ([molecule]
   (process-molecule molecule (parse-rules replacements)))
  ([molecule rules]
   (set (flatten (for [[old new] rules] (all-replacements molecule old new))))))

(count (process-molecule molecule))

;;; part 2

(defn make-graph [replacements]
  (apply merge (for [[k v] (group-by first replacements)]
                 {k (mapv second v)})))

(defn breadth-first-search [start next-moves is-goal?]
  (let [q (clojure.lang.PersistentQueue/EMPTY)]
    (loop [open (conj q start)
           closed #{}]
      (println (peek open))
      (cond (is-goal? (peek open)) (peek open)
            (closed (peek open)) (recur (pop open) closed)
            :else (recur (apply conj (pop open) (next-moves (peek open)))
                         (conj closed (peek open)))))))

(breadth-first-search "e" (make-graph (parse-rules replacements)) #(= % molecule))
((make-graph (parse-rules replacements)) "e")

(defn factors [n] (filter #(= 0 (mod n %)) (range 1 (inc n))))
(first (filter #(= (/ 36000000 10) (reduce + (factors %)))
               (range)));259200
(group-by even? (range 100))


(defn odd-pyramid-sum [n]
  (if (= n 1) 1 (+ (* 2 n) (odd-pyramid-sum (dec n)))))

(odd-pyramid-sum 4)

(def blah [[:a 2] [:b 3] [:a 4] [:c 3] [:b 100]])

(apply hash-map (mapcat (fn [[k v]] [k (map second v)]) (group-by first blah)))
(reduce (fn [m [k v]] (assoc m k (conj (get m k []) v)) ) {} blah)
(reduce (fn [m [k v]] (update m k (fnil conj []) v) ) {} blah)
(into {} [[:a 1] [:b 2]])
(conj {} [:b 2] [:a 1])
(let [[[x y] z] [["me x" "me y"] "me z"]] [x y z])
