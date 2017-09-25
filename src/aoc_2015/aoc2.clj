(require '[clojure.string :as str])

(def data (slurp "aoc2.txt"))

(defn dims-to-area [dims]
  (let [[l w h ] 
    (map read-string (str/split dims #"x"))]
    [(* l w)
     (* w h)
     (* l h)]))

(defn total-sq-ft [dims]
  (let [d (dims-to-area dims)]
  (+
    (apply min d)
    (apply + (map (partial * 2) d)))))

;part 1
(apply + (map total-sq-ft (str/split data #"\n")))

(defn ribbon-size [dims]
  (let [d (map read-string(str/split dims #"x"))]
    (+
      (apply + (map (partial * 2) (drop-last(sort d))))
      (apply * d))))

;part 2
(apply + (map ribbon-size (str/split data #"\n")))




