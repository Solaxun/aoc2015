(def data (slurp "aoc1.txt"))

(def direction {\( 1 \) -1})

(apply + (map direction data))

(count
  (take-while (partial <= 0)
            (reductions #(+ %1 (direction %2)) 0 data)))







