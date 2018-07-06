(ns aoc-2015.aoc23
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.repl :refer [source]]
            [clojure.java.io :as io]))

(def instructions (str/split-lines (slurp (io/resource "aoc23.txt"))))

(defn parse-instructions [instr {a "a" b "b" :as regs}]
  (loop [[pos regs] [0 regs]]
    (if (or (< pos 0) (> pos (dec (count instr))))
      regs
      (let [[op reg & jump] (str/split (instr pos) #" ")]
        (recur (case op
                 "hlf" [(inc pos) (update regs reg / 2)]
                 "tpl" [(inc pos) (update regs reg * 3)]
                 "inc" [(inc pos) (update regs reg inc)]
                 "jmp" [(+ pos (read-string reg)) regs]
                 "jie" (if (even? (-> reg first str regs))
                         [(+ pos (read-string (first jump))) regs]
                         [(inc pos) regs])
                 "jio" (if (= 1 (-> reg first str regs))
                         [(+ pos (read-string (first jump))) regs]
                         [(inc pos) regs])))))))
;; part one
(parse-instructions instructions {"a" 0 "b" 0})
;; part two
(parse-instructions instructions {"a" 1 "b" 0})
