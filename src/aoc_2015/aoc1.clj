(def data (slurp "src/aoc_2015/aoc1.txt"))

(def direction {\( 1 \) -1})

;part 1
(apply + (map direction data))

;part 2
(count
 (take-while #(<= 0  %1) (reductions #(+ %1 (direction %2)) 0 data)))



