(ns aoc-2015.aoc19
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.set :as set]
            [clojure.repl :refer [source]]))

(def data (str/split-lines (slurp "src/aoc_2015/aoc19.txt")))
(def molecule (last data))
(def replacements (drop-last 2 data))

(defn parse-rules [rules]
  (map (fn [rule] (str/split rule #" => ")) rules))

(defn index-of-occurences [word target]
  (set (remove nil?
               (map-indexed (fn [i x] (clojure.string/index-of word target i))
                            word))))

(defn all-replacements [word target replacement]
  (let [all-indexes (index-of-occurences word target)]
    (map (fn [i]  (str (subs word 0 i)
                      replacement
                      (subs word (+ i (count target)))))
         all-indexes)))

(defn process-molecule
  ([molecule]
   (process-molecule molecule (parse-rules replacements)))
  ([molecule rules]
   (set (flatten (for [[old new] rules] (all-replacements molecule old new))))))

(count (process-molecule molecule))

;;; part 2

(defn a-star-search [start next-moves cost-fn is-goal?]
  "at once.. for all successors if haven't seen and new cost cheaper, save new
  cost and put neighbor on priority queue with new_cost + est cost to goal"
  (let [q (sorted-set)]
    (loop [open (conj q [0 start])
           closed {}]
      (let [[prev-cost path :as current] (first open)
            curr-cost (inc prev-cost)
            remain-cost (cost-fn path)
            total-cost (+ curr-cost remain-cost)]
        ;(println current)
        (cond
             ;; case 1
             (is-goal? path)
              path
             ;; case 2
              (contains? closed path)
              (if (< curr-cost (closed path))
                (recur (conj open [total-cost path]) (assoc closed path curr-cost))
                (recur (disj open current) closed))
             ;; case 3
              :else (recur (into (disj open current)
                                 (mapv #(vector (inc curr-cost) %)
                                       (next-moves path)))
                           (assoc closed path curr-cost)))))))


(a-star-search "e" process-molecule #(string-dist % molecule) #(= % molecule))
(cost-fn 1 "ORnPB" molecule)

(defn breadth-first-search [start next-moves is-goal?]
  (let [q (clojure.lang.PersistentQueue/EMPTY)]
    (loop [open (conj q start)
           closed #{}]
      (cond (is-goal? (peek open)) (peek open)
            (closed (peek open)) (recur (pop open) closed)
            :else (recur (apply conj (pop open) (next-moves (peek open)))
                         (conj closed (peek open)))))))

(defn schedule
  [& {:keys [k lam limit] :or {k 20 lam 0.005 limit 100}}]
  (fn [t] (if (< t limit) (* k (Math/exp (* t (- lam) ))) 0)))

(defn string-dist [s1 s2]
  (let [s1 (concat s1 (repeat nil))
        same-posn (count (remove false? (map #(not= %1 %2) s1 s2)))
        ;size-diff (- (count s2) (count s1))
        ]
    ;(- size-diff (* 2 same-posn))
    same-posn))


(defn simulated-annealing
  "Randomly select neighbors to move to, with decreasing frequency as
  temperature drops. Temperature should  decrease over time, and as that
  happens the resultant value of e^-delta/t will approach zero.

  Finally, once the value of e^-delta/temp is determined, it is compared
  to a uniformly random number between 0 and 1.  If the value exceeds this
  random number, we switch.

  Example of how temperature impacts probability of switching, holding delta
  constant:

    exp(-0.01) ~= 0.99  - almost certainly switch (likely > random(0,1)
    exp(-0.99) ~= 0.37  - likely not switch (likely < rand(0,1)

  One useful heuristic for setting an initial temperature is to estimate
  the maximum absolute delta of the cost-fn.  In this implementation, that
  temperature is represented by the constant 'k' to the schedule function.
  "

  [start get-neighbors cost-fn schedule goal?]
  (loop [node start iter 1]
    (let [old-cost (cost-fn node)
          neighbors (-> node get-neighbors vec)
          temp (schedule iter)] ;; get exponentially smaller
      (cond (or (empty? neighbors) (zero? temp)) ["so-far" node]
            (goal? node) ["solution" node]
            :else (let [neighbor (rand-nth neighbors)
                        new-cost (cost-fn neighbor)
                        delta    (- new-cost old-cost)]
                    (println old-cost new-cost delta temp (Math/exp (/ (- delta) temp)))
                    (recur (if (or (< delta 0)
                                   (< (rand 1.0) (Math/exp (/ (- delta) temp))))
                             neighbor
                             node)
                           (inc iter)))))))

(simulated-annealing "e"
                     process-molecule
                     #(string-dist % molecule)
                     (schedule :k 200 :limit 1000)
                     #(= molecule %))

(defn hill-climb
  "take the best move we can at every step; if none better quit"
  [start get-neighbors cost-fn goal? max-tries]
  (loop [node start i 1]
    (println (count node) node)
    (if (or (> i max-tries) (empty? (get-neighbors node)))
      ["fail" node]
      (let [neighbor (apply min-key cost-fn (get-neighbors node))
            old-cost (cost-fn node)
            new-cost (cost-fn neighbor)]
        (println (count (get-neighbors node)))
        (if (<= new-cost old-cost)
          (recur neighbor (inc i))
          ["none cheaper" node])))))

(hill-climb "e"
            process-molecule
            #(string-dist % molecule)
            #(= molecule %)
            1e4)

;; find largest key at each step
;; replace it with max drop in size

;; neighbor will be all replacements of molecule
(defn reverse-graph [replacements]
  (apply merge (for [[k v] (parse-rules replacements)]
                 {v k})))

(keys (reverse-graph replacements))

(defn process-molecule
  ([molecule]
   (process-molecule molecule (reverse-graph replacements)))
  ([molecule rules]
   (set (flatten (for [[old new] rules] (all-replacements molecule old new))))))

(hill-climb molecule
            process-molecule
            count
            #(= "e" %)
            1e3)

(simulated-annealing molecule
                     process-molecule
                     count
                     (schedule :k 200 :limit 1000)
                     #(= "e" %))

(breadth-first-search molecule process-molecule #(= % "e"))
(a-star-search molecule process-molecule count #(= %  "e"))


(defn random-search
  "take the best move we can at every step; if none better quit"
  [start get-neighbors cost-fn goal? max-tries]
  (loop [node start i 0]
    ;(println (count node) node)
    (if (or (> i max-tries) (empty? (get-neighbors node)))
      [node i]
      (recur (rand-nth (-> node get-neighbors vec)) (inc i)))))

(filter #(= "e" (second %))
        (for [i (range 20)] (random-search molecule process-molecule count #(= % "e") 1e3)))


(defn a-star-search [start neighbor-func goal? remain-cost path-cost]
  (loop [q (conj (sorted-set) [0 start])
         cost-so-far {start 0}
         came-from   {start nil}]
    (if-let [[node-cost node] (first q)]
      (if (goal? node) node
          (let [neighbors (neighbor-func node)
                prev-node (came-from node)
                prev-cost (cost-so-far node)
                cheaper (remove #(< (cost-so-far % 1e6)
                                    (+ prev-cost (path-cost node %)))
                                neighbors)
                new-nodes (map #(vector (+ prev-cost
                                           (path-cost node %)
                                           (remain-cost %)) %)
                               cheaper)]
            (recur (into q new-nodes)
                   (->> cheaper
                        (map #(vector % (+ prev-cost (path-cost node %))))
                        (into cost-so-far))
                   (into came-from (map (juxt identity (fn [_] node)) new-nodes)))))
      "no more neigbors")))

(a-star-search molecule process-molecule #(= %  "e") count (fn [prev cur] 1))
