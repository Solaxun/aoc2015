(defn cycle-pw [password res]
  (if (empty? password) [res] 
      (mapcat #(cycle-pw (rest password) (str res (char (+ 96 %))))
           (range (alpha-map (first password)) 27 ))))

(defn cycle-pw [password]
  (loop [pw password
         n (dec(count pw))
         res [pw]]    
    (cond
     ;; case one (base)
     (= n -1) res
     ;; case two
     (= (nth pw n) \z)
     (let [newpw (str(subs pw 0 n) 
                     (apply str (repeat (count (subs pw n)) "a")))]
       (recur newpw (dec n) res))
     ;; case 3
     :else (let [newpw (str(subs pw 0 n) 
                           (char(inc(int(nth pw n)))) 
                           (subs pw (inc n)))]
             (recur newpw (dec(count pw)) (conj res newpw))))))
