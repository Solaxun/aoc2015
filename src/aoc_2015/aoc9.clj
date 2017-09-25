(ns aoc-2015.aoc9
  (require [clojure.set :refer :all]
           [clojure.string :as str]
           [clojure.java.io :as io]
           [clojure.math.combinatorics :as combo]))

(def data (str/split (slurp "src/aoc_2015/aoc9.txt") #"\n"))

(defn parse-dirs [dir]
  (rest (re-find  #"(\w+) to (\w+) = (\d+)" dir)))

(def unique-cities (into #{} (mapcat #(take 2 %) (map parse-dirs data))))

(def all-visits (combo/permutations unique-cities))

(defn dir->map [[start end dist]] (hash-map [start end] dist
                                            [end start] dist))

(defn make-lkp [input]
  (->> input
       parse-dirs
       dir->map))

(def lkp-table (apply merge (map make-lkp data)))

(defn trip-distance [trip]
  (->> trip
       (partition 2 1)
       (map lkp-table)
       (map read-string)
       (reduce +))) 

(apply #_ min max (map trip-distance all-visits))

















