(ns aoc-2015.aoc5
  (require [clojure.string :as str]
           [clojure.set :refer :all]))

(def data (str/split (slurp "src/aoc_2015/aoc5.txt") #"\n"))

;not that pretty
(defn nice1 [word]
  (every? true? ((juxt #(<= 3 (count(re-seq #"[aeiou]" %1)))
         			   #(boolean(re-find #"(\w)(\1)" %1))
         	           #(not(boolean(re-find #"(ab|cd|pq|xy)" %1)))) word)))

;part 1
(count(filter nice1 data))

(defn nice2 [word]
  (every? not-empty 
    ((juxt
       (partial re-find #"(\w)\w(\1)") 
       (partial re-find #"(\w{2}).*\1"))word)))

;part 2
(count (filter nice2 data))

; (every? true? (map not-empty [[] ["qjhvhtzxzqqj" "qj"]]))