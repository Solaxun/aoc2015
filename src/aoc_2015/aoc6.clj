(ns aoc-2015.aoc6
  (require [clojure.string :as str]
           [clojure.set :refer :all]))

(def data (str/split (slurp "src/aoc_2015/aoc6.txt") #"\n"))

(defn lights [n]
  (reduce into {}
          (partition n (for [i (range n) j (range n)] [[i j], 0]))))

;not particularly thrilled with this way of generating points between
(defn points-between [[start end]]
  (let [[x1 y1] start
        [x2 y2] end]
      (for [x (range x1 (inc x2))
            y (range y1 (inc y2))]
          [x y])))

(defn off [_] 0)
(defn on [_] 1)
(defn toggle [x] (if (= 0 x) 1 0))

(defn update-keys [m keys update-func]
  (reduce #(assoc %1 %2 (update-func (m %2))) m keys))

(def action "turn on 943,300 through 990,907")

(defn do-action[lights [action points]]
  (cond (= action "turn on") (update-keys lights (points-between points) on)
        (= action "turn off") (update-keys lights (points-between points) off)
        :else (update-keys lights (points-between points) toggle)))

(defn parse [action]
  (->> action
       (re-seq #"(toggle|turn on|turn off)\s(\d+,*\d+)\sthrough\s(\d+,*\d+)")
       (apply rest)
       (map #(str/split % #","))
       flatten
       (#(conj [(first %)] (partition 2 (map read-string(rest %)))))))
;part1
(count
  (filter #(= 1 (second %1))
           (reduce #(do-action %1 %2) (lights 1000) (map parse data))))

;part two just change update-funcs and use below for final result
;(apply + (vals(reduggce #(do-action %1 %2) (lights 1000) (map parse data))))




     
