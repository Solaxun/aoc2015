(ns aoc-2015.aoc20
  (:require [clojure.repl :refer [source]]
            [clojure.math.combinatorics :]))

; repeats last num for  perfect squares, too lazy to fix
(defn fast-factors [n]
  (->> (range 2 (int (inc (Math/sqrt n))))
       (mapcat #(if (zero? (mod n %)) [% (/ n %)]))
       (apply hash-set 1 n)))

;; about 30 seconds
(some #(when (>= (* (apply + (fast-factors %))) 3600000) %) (range))

(defn fast-factors-limit-50 [n]
  (remove #(< 50 (/ n %)) (fast-factors n)))

;; ~90 seconds
(some #(when (>= (* 11 (apply + (fast-factors-limit-50 %))) 36000000) %) (iterate inc 1))
