(require '[digest :refer [md5]])

(def salt "ckczppom")

(defn valid? [code,zeroes]
  (= (subs code 0 6) zeroes))

(defn part1 [salt i zeroes]
  (loop [i 1]
    (cond (valid? (md5 (str salt i)) zeroes) i
          :else (recur (inc i)))))

; (part1 salt 1 "00000")
; (part1 salt 1 "000000") 

;or alternatively
(defn part2 [salt zeros count-zeros]
  (first
    (for [i (range)
          :when (= (subs (md5 (str salt i)) 0 count-zeros) zeros)]
      i)))

; (part2 salt "000000" 6)

(take 10 (map-indexed #(str %2 %1) (repeat salt)))