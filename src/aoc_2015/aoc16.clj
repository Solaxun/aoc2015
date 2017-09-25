(ns aoc-2015.aoc16
    (:require[clojure.string :as str]))

(def target-sue
  {"children" "3"
   "cats" "7"
   "samoyeds" "2"
   "pomeranians" "3"
   "akitas" "0"
   "vizslas" "0"
   "goldfish" "5"
   "trees" "3"
   "cars" "2"
   "perfumes" "1"})

(def all-sues (slurp "src/aoc_2015/aoc2016.txt"))

(defn parse-sue [sue]
  (let [sue-num    (map rest(re-seq #"(Sue) (\d+)" sue))
        sue-things (map rest(re-seq #"(\w+): (\d+)" sue))]
    (into {} (mapv vec (cons (first sue-num) sue-things)))))

(defn merge-with-kv [f m1 m2]
  (reduce (fn [m1 [k2 v2]] (if (m1 k2)
                            (conj m1 [k2 (f k2 (m1 k2) v2)])
                            (conj m1 [k2 v2])))
            m1 m2))
;; part 1
#_(defn matching-elements [target-sue current-sue]
  (filter (fn[[k v]] (= true v))
          (merge-with #(= %1 %2) current-sue target-sue)))
;; part 2
(defn matching-elements [target-sue current-sue]
  (filter (fn[[k v]] (= true v))
          (merge-with-kv (fn [k v1 v2] (case k
                                   ("pomeranians" "goldfish") (< (read-string v1)
                                                                 (read-string v2))
                                   ("cats" "trees")           (> (read-string v1)
                                                                 (read-string v2))
                                   (= v1 v2)))
                                 current-sue target-sue)))

(defn is-aunt-sue?
  ([current-sue] (is-aunt-sue? current-sue target-sue))
  ([current-sue target-sue]
   (if (= 0
          (- (count (matching-elements target-sue current-sue))
             (dec (count current-sue))))
     current-sue
     false)))

(filter boolean
        (map is-aunt-sue?
             (map parse-sue (str/split all-sues #"\n"))))
