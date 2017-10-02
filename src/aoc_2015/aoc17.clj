(ns aoc-2015.aoc17
  (:require [clojure.math.combinatorics :as combs]))

(def buckets
  [11 30 47 31 32 36 3 1 5 3 32 36 15 11 46 26 28 1 19 3])

(defn subsets-with-dups [coll]
  (for [subset (combs/subsets (map-indexed vector coll))] ; use index to make each element unique
    (map last subset))) ; then for each index,element pair, take only the element

;; pt 1
(count (filter #(= 150 (reduce + %)) (subsets-with-dups buckets)))
;; pt 2
(->> buckets
     subsets-with-dups
     (filter #(= 150 (reduce + %)))
     ; uncomment below 3 lines to get min # buckets that fit 150 (and comment last line)
     #_(sort-by count)
     #_first
     #_count
     (filter #(= (count %) 4))) ; now we know min buckets is 4, so how many of that size can fit 150?
