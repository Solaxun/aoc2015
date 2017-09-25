
(ns aoc-2015.aoc15
  (require [clojure.java.io :as io]
           [clojure.set :refer :all]
           [clojure.string :as str]
           [clojure.repl :refer [source]]
           [clojure.math.combinatorics :as combo]
           [clojure.data.json :as json]))
           
(def data
"Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3
Butterscotch: capacity 0, durability 5, flavor -3, texture 0, calories 3
Chocolate: capacity 0, durability 0, flavor 5, texture -1, calories 8
Candy: capacity 0, durability -1, flavor 0, texture 5, calories 8")

(def data2
"Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")

(def split-ingreds (str/split data #"\n"))

(defn props->map [props]
  (->> props
       (apply #(re-seq #"\w+ -*\d+" %))
       (map #(str/split % #" "))
       drop-last ; dropping calories here, fix this for pt 2
       (map (juxt first (comp read-string last)))
       (into {})))

(defn line->ingredmap [text]
  (let [ingredients (str/split text #": ")
        ingredient (first ingredients)
        props (rest ingredients)
        propsmap (props->map props)]
        {ingredient propsmap}))

(def ingredmap (apply merge (map line->ingredmap split-ingreds)))

;; this is not what i thought it is.. it's cartesian product w/ a repeat
(defn ingredient-combos [ingredients]
  (combo/selections ingredients 100))

(defn combs-with-replacement 
  ([xs n] 
   (combs-with-replacement [] xs n))
  ([combs xs n]
   (if (zero? n) [combs]
       (apply concat
              (map-indexed
               (fn [i x]
                 (combs-with-replacement (conj combs x)
                                         (subvec xs i)
                                         (dec n)))
               xs)))))

(defn score [mix]
  (->> mix
       frequencies
       (map (fn [[k v]] (map (partial * v) (vals (ingredmap k)))))
       (apply map +)
       (reduce #(* %1 (max 0 %2)) 1)))

(def ingredients ["Sprinkles" "Butterscotch" "Chocolate" "Candy"])

(->> ingredients
     (#(combs-with-replacement % 100))
     (map score)
     (apply max))

 
