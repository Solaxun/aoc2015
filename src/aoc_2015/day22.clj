(ns aoc-2015.aoc22
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.repl :refer [source]]))

(def BOSS   {:player "boss" :hp 71 :dmg 10 :effects {}})
(def PLAYER {:player "player" :hp 50 :mana 500 :armor 0 :effects {} :spent 0})
(def spells {"poison" 173 "recharge" 229 "drain" 73 "magic-missle" 53 "shield" 113})

;; processing separate from casting - this is the tick effect only
(defmulti effect (fn [spell p c] spell))

(defmethod effect "shield"   [_ p c]
  (if (pos? c)
    (-> p
        (assoc-in [:effects "shield"] (dec c))
        (assoc :armor 7))
    (assoc p :armor 0))) ;; need to remove armor once counter done

(defmethod effect "poison"  [_ p c]
  (if (pos? c)
      (-> p (update-in [:effects "poison"] dec) (update :hp - 3))
      p))

(defmethod effect "recharge" [_ p c]
  (if (pos? c)
    (-> p (update-in [:effects "recharge"] dec) (update :mana + 101))
    p))

(defn process-effects [player]
  (reduce-kv (fn [player spell counter] (effect spell player counter))
             player
             (:effects player)))

;; actual casting
(defmulti cast-spell (fn [spell-name _ _] spell-name))

(defmethod cast-spell "poison" [_ p b]
  (let [p (-> p (update :mana - 173) (update :spent + 173))
        b (-> b (update :effects conj {"poison" 6}))]
      [p b]))

(defmethod cast-spell "drain" [_ p b]
  (let [p (-> p (update :mana - 73) (update :hp + 2) (update :spent + 73))
        b (-> b (update :hp - 2))]
      [p b]))

(defmethod cast-spell "shield" [_ p b]
  (let [p (-> p (update :mana - 113) (update :effects conj {"shield" 6}) (update :spent + 113))
        b b]
      [p b]))

(defmethod cast-spell "magic-missle" [_ p b]
  (let [p (-> p (update :mana - 53)(update :spent + 53))
        b (-> b (update :hp - 4))]
      [p b]))

(defmethod cast-spell "recharge" [_ p b]
  (let [p (-> p (update :mana - 229) (update :effects conj {"recharge" 5})(update :spent + 229))
        b b]
      [p b]))

(defn available-spells [p spells]
  (->> spells
      (remove (fn [[spell cost]] (> cost (:mana p))))
      (remove (fn [[spell cost]] (pos? (get-in p [:effects spell] 0))))
      (into {})))

(declare boss-turn)

(defn player-turn [player boss]
  ;(println player)
  ;(println boss)
  (let [player (update player :hp dec)
        player (process-effects player)
        boss   (process-effects boss)
        spells (keys (available-spells player spells))]
    (cond (<= (:hp boss) 0) ["win" player]
          (or (<= (:hp player) 0) (empty? spells)) ["lose" player]
          :else (mapcat (fn [spell-name] (boss-turn (cast-spell spell-name player boss))) spells))))

(defn boss-turn [[player boss]]
  (let [player (process-effects player)
        boss   (process-effects boss)]
    (if (<= (:hp boss) 0)
      ["win" player]
      (player-turn
       (update player :hp - (max 1 (- (:dmg boss) (:armor player))))
       boss))))

;;(cast-spell "shield" PLAYER BOSS)
(apply min (map (fn [[outcome player]] (:spent player))
                (filter #(= "win" (first %)) (partition 2 (player-turn PLAYER BOSS)))))
