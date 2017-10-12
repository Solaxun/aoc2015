(ns aoc-2015.aoc18
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]))

(def data (str/split (slurp "src/aoc_2015//aoc18.txt") #"\n"))

(def grid (vec (map vec data)))

(defn neighbor-locs []
  (for [i [-1 0 1] j [-1 0 1]
        :when (not (and (= i 0) (= j 0)))] [i j])) ; don't include start pos [0,0]

(defn get-neighbors [board loc]
  (filter #(= \# %)
          (map #(get-in board (map + % loc)) (neighbor-locs))))

(defn turn-loc-on? [board loc]
    (let [on-count (count (get-neighbors board loc))
        cur-pos (get-in board loc)]
    (if (= cur-pos \#) ; if on, stay on if 2 or 3 neighbors on, else off
      (or (= on-count 2)
          (= on-count 3)
          (some #{loc} [[0 0] [0 99] [99 0] [99 99]])) ; pt 2
      (= on-count 3)))) ; if off, turn on if exactly 3 neighbors are on, else off

(defn next-board [board]
  (vec (map-indexed (fn [r row]
                      (vec (map-indexed (fn [c col] (if (turn-loc-on? board [r c])
                                                     \#
                                                     \.)) row))) board)))


(defn count-lights-on [board]
  (count (mapcat (fn [row] (filter #(= \# %) row)) board)))

(count-lights-on (nth (iterate next-board grid) 100))

(defn update-vals [grid ks]
  (reduce #(assoc-in %1 %2 \#) grid ks))
(count-lights-on (nth (iterate next-board (update-vals grid [[0 0] [0 99] [99 0] [99 99]])) 100))
