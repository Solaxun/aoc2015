(ns aoc-2015.aoc8
  (require [clojure.string :as str]
           [clojure.set :refer :all]
           [clojure.java.io :as io]))

(def data (str/split (slurp "src/aoc_2015/aoc8.txt") #"\n"))

(defn remove-hex [n s] (str/replace s  #"\\x\w{2}" (apply str (repeat n "|")))) 
(defn remove-single-slash [n s] (str/replace s #"\\" (apply str (repeat n "|")))) 
(defn remove-lone-quote [n s]  (str/replace s #"\"" (apply str (repeat n "|"))))

(defn remove-double-slash [n s] (str/replace s #"\\\\" (apply str (repeat n "|")))) 
(defn remove-slash-quote [n s]  (str/replace s "\\\"" (apply str (repeat n "|"))))

(defn code-length [s]
  (->> s
      (drop 1)
      (drop-last)
      (apply str)
      (remove-hex 4)
      (remove-single-slash 1)
      (remove-lone-quote 1)))

(defn string-length [s]
  (->> s
      (drop 1)
      (drop-last)
      (apply str)
      (remove-double-slash 1)
      (remove-hex 1)
      (remove-slash-quote 1)))
 
(defn length-diffs [s]
  (- (+ 2 (count (code-length s)))
     (count (string-length s))))

(reduce + (map length-diffs data))

;; part 2
;each sp char counts as two, reg char as one, then +2 for new quotes
(def spchars {\\ 2, \" 2})

(defn expand-chars [s]
  (reduce + 2 (map #(get spchars  %1 1) s)))

(reduce + (map #(- %1 %2) (map expand-chars data)
               (map (comp (partial + 2) count code-length) data)))
        
