(ns aoc-2015.aoc7
  (require [clojure.java.io :as io]
           [clojure.set :refer :all]
           [clojure.string :as str]
           [clojure.repl :refer [source]]
           [clojure.math.combinatorics :as combo]
           [clojure.data.json :as json]))

(def data (str/split (slurp "src/aoc_2015/aoc7.txt") #"\n"))

(def instructions
  ["123 -> x"
  "456 -> y"
  "x AND y -> d"
  "x OR y -> e"
  "x LSHIFT 2 -> f"
  "y RSHIFT 2 -> g"
  "NOT x -> h"
  "NOT y -> i"])

;thanks internet
(defn bit-not-unsigned [num]
  (->> num
     bit-not
     (bit-and 16rFFFF)))

(defn assign-to [[x _ variable] reg]
  (assoc reg variable (get-val reg x)))
  
(defn assign-not [[op x _ variable] reg]
  (assoc reg variable (bit-not-unsigned (get-val reg  x))))

(defn assign-and-or [[x op y _ variable] reg]
  (case op
      "AND"    (assoc reg variable (bit-and (get-val reg  x) (get-val reg y)))
      "OR"     (assoc reg variable (bit-or (get-val reg  x) (get-val reg y)))
      "LSHIFT" (assoc reg variable (bit-shift-left (get-val reg x) (get-val reg y)))
      "RSHIFT" (assoc reg variable (bit-shift-right (get-val reg x) (get-val reg y)))))

(defn get-val [m x]
  (m x (read-string x)))
    
(defn parse [instruction registers]
  ;; (println instruction)
  (let [inst (re-seq #"\w+|->" instruction)]
       (case (count inst)
             3 (assign-to inst registers)
             4 (assign-not inst registers)
             5 (assign-and-or inst registers))))

(def registers
  (let [k (map (comp last last #(re-seq #"-> (\w+)" %)) data)]
    (zipmap k (repeat (count k) 0))))

(def registers2 (zipmap ["x" "y" "d" "e" "f" "g" "h" "i"] (repeat 8 0)))
;; need to only assign values when both inputs are present, redo
(reduce (fn [acc curr] (parse curr acc)) registers data)
