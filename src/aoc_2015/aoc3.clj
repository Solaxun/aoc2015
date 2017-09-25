(ns aoc-2015.aoc3
  (require [clojure.set :refer :all]))

(def data (slurp "src/aoc_2015/aoc3.txt"))
(def dirs {\^ [1 0]
           \v [-1 0]
           \> [0 1]
           \< [0 -1]})

(def points (map dirs data))

(defn add-coords [c1 c2] (map + c1 c2))

;part 1
; (count(set(reductions add-coords [0,0] points)))

;part 2
(def santa-dirs (take-nth 2 data))
(def robot-dirs (take-nth 2 (drop 1 data)))

(defn locations-visited [data] 
  (->> data
       (map dirs)
       (reductions add-coords [0,0])
       (set)))

(count (union (set (locations-visited robot-dirs))
 			  (set (locations-visited santa-dirs))))







