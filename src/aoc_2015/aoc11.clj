(ns aoc-2015.aoc11
  (require [clojure.java.io :as io]
           [clojure.set :refer :all]
           [clojure.string :as str]
           [clojure.repl :refer [source]]
           [clojure.math.combinatorics :as combo]))

(def password "cqjxjnds")

(defn two-overlap? [pw]
  ((comp #(> % 1) count #(get-repeats % "" #{})) pw))

(defn bad-letters? [pw]
  (boolean (some #{\i\o\l} pw)))
  
(defn three-increasing? [pw]
  (some true? (for [[x y z] (partition 3 1 (map int pw))]
                (and (= 1 (- y x) (- z y))))))

(defn valid-password? [pw]
  (every? true?
          ((juxt (complement bad-letters?) two-overlap? three-increasing?)
           (apply str pw))))

(defn get-repeats [chars prev results]
  (let [f (first chars)
        s (second chars)
        r (rest chars)]
    (cond (empty? chars) results
          (and (= f s) (not= f prev))
          (recur (rest chars) f (conj results f))
          :else (recur (rest chars) f results))))

(defn cycle-pw
  ([pw] 
   (cycle-pw pw (dec(count pw))))
  ([pw n]
   (cond ;; case one (base)
    (= n -1) nil
    ;; case two
    (= (nth pw n) \z)
    (let [newpw (str(subs pw 0 n) 
                    (apply str (repeat (count (subs pw n)) "a")))]
      (cycle-pw newpw (dec n)))
    ;; case 3
    :else (let [newpw (str(subs pw 0 n) 
                          (char(inc(int(nth pw n)))) 
                          (subs pw (inc n)))]
            (cons newpw (lazy-seq (cycle-pw newpw (dec(count pw)))))))))

(#_first second (filter valid-password? (cycle-pw password)))

