(ns aoc-2015.aoc21
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.set :refer [intersection]]
            [clojure.repl :refer [source]]))

(def BOSS {:player "boss" :hp 104 :dmg 8 :armor 1})
(def PLAYER {:player "player" :cost 0 :hp 100 :dmg 0 :armor 0})

(def weapons {:dagger {:cost 8 :dmg 4 :armor 0}
              :shortsword {:cost 10 :dmg 5 :armor 0}
              :warhammer {:cost 25 :dmg 6 :armor 0}
              :longsword {:cost 40 :dmg 7 :armor 0}
              :greataxe {:cost 74 :dmg 8 :armor 0}})

(def armor   {:leather {:cost 13 :dmg 0 :armor 1}
              :chaimnail {:cost 31 :dmg 0 :armor 2}
              :splintmail {:cost 53 :dmg 0 :armor 3}
              :blademail {:cost 75 :dmg 0 :armor 4}
              :platemail {:cost 102 :dmg 0 :armor 5}
              :empty {:cost 0 :dmg 0 :armor 0}})

(def rings   {:dmg+1 {:cost 25 :dmg 1 :armor 0}
              :dmg+2 {:cost 50 :dmg 2 :armor 0}
              :dmg+3 {:cost 100 :dmg 3 :armor 0}
              :def+1 {:cost 20 :dmg 0 :armor 1}
              :def+2 {:cost 40 :dmg 0 :armor 2}
              :def+3 {:cost 80 :dmg 0 :armor 3}
              :empty {:cost 0 :dmg 0 :armor 0}})

(defn equip-player [w a r1 r2]
  (let [w (get weapons w)
        a (get armor a)
        r1 (get rings r1)
        r2 (get rings r2)
        player (merge-with + w a r1 r2 PLAYER)]
    player))

(defn play-game [p1 p2]
  (loop [p1 p1
         p2 p2]
    (if (<= (:hp p1) 0) p2 ;; if player dies, other wins
        (recur (update p2 :hp - (max 1 (- (:dmg p1) (:armor p2))))
                       p1))))

(apply #_min max (for [w (keys weapons)
                 a (keys armor)
                 r1 (keys rings)
                 r2 (keys rings)
                 :when (not= r2 r1)
                 :let [player (equip-player w a r1 r2)
                       winner (play-game player boss)
                       pcost  (:cost player)
                       winner-name (:player winner)]]
             (if (= winner-name #_"player" "boss")
               #_(:cost winner) pcost
               #_1e9 0)))
