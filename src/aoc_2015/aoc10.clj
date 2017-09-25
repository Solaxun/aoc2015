(ns aoc-2015.aoc10
  (require [clojure.java.io :as io]
           [clojure.set :refer :all]
           [clojure.string :as str]))

(def data "3113322113")

(defn encode-string [data results count]
   (let [prev (first data)
         curr (second data)]
     (cond (empty? data) results
           (not= prev curr)
           (recur (rest data) (str results count prev) 1)
           :else (recur (rest data) results (inc count)))))
;takes ~4 hours for 50, rewrite to not build str on each iteration
(count
 (nth (iterate #(encode-string % "" 1) data) #_40 50))
;delete indentation (M - ^)
;go to first non whitespace in line (M - m )


(group-by identity "abbbccda")
