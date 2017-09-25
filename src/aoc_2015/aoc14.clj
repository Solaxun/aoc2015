(ns aoc-2015.aoc14
  (require [clojure.string :as str]))

(def reindeer-input
  "Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.
   Blitzen can fly 13 km/s for 4 seconds, but then must rest for 49 seconds.
   Rudolph can fly 20 km/s for 7 seconds, but then must rest for 132 seconds.
   Cupid can fly 12 km/s for 4 seconds, but then must rest for 43 seconds.
   Donner can fly 9 km/s for 5 seconds, but then must rest for 38 seconds.
   Dasher can fly 10 km/s for 4 seconds, but then must rest for 37 seconds.
   Comet can fly 3 km/s for 37 seconds, but then must rest for 76 seconds.
   Prancer can fly 9 km/s for 12 seconds, but then must rest for 97 seconds.
   Dancer can fly 37 km/s for 1 seconds, but then must rest for 36 seconds.")

(def reindeer-sample
  "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
   Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")

(def reindeer (map str/trim (str/split reindeer-input #"\n")))

(defn parse [reindeer]
  (let [r (str/split reindeer #" ")]
    (map read-string 
         ((juxt first 
                #(nth % 3) 
                #(nth % 6) 
                #(last (drop-last %)))
              r))))

(def deernames (map first (map parse reindeer)))
(def deerscores (zipmap deernames (repeat (count deernames) 0)))

(defn sprinting [time-left sprint-time rest-time pace traveled]
  (if (<= (- time-left sprint-time) 0)
    (+ traveled (* time-left pace))
    (resting (- time-left sprint-time) 
             sprint-time 
             rest-time
             pace
             (+ traveled (* sprint-time pace)))))
    
(defn resting [time-left sprint-time rest-time pace traveled]
  (if (<= (- time-left rest-time) 0)
    traveled
    (sprinting (- time-left rest-time) 
               sprint-time 
               rest-time 
               pace 
               traveled)))    
          
(defn distance-traveled
  [[reindeer pace sprint-time rest-time] time-left traveled]
  {reindeer (sprinting time-left sprint-time rest-time pace 0)})

(defn part1 []        
  (->> reindeer
       (map parse)
       (map #(distance-traveled % 2503 0))
       (mapcat vals)
       (apply max)))

(defn winner [deermaps]
  ;;takes collection of {deer dist}, returns one w/ highest dist
  ;;FIXME - can't return just max, bc need to increment both on ties
  (reduce 
    (fn [m1 m2] 
        (if (> (first (vals m1)) (first (vals m2))) m1 m2))
      deermaps))
    
(defn round-winner [round]        
  (->> reindeer
       (map parse)
       (map #(distance-traveled % round 0))
       winner
       ffirst))
     
(sort-by last (frequencies (map round-winner (range 1 2504))))

