(ns change)

(defn update-tables [num cns i cs]
  (reduce (fn [[upd-num upd-cns] c]
            (let [tk (inc (get upd-num (- i c) ##Inf))
                  no (get upd-num i ##Inf)]
              (if (< tk no)
                [(assoc upd-num i tk) (assoc upd-cns i c)]
                [upd-num upd-cns])))
          [num cns]
          cs))

(defn build-tables [a cs]
  (loop [i   0
         num {0 0}
         cns {0 nil}]
    (if (< a i)
      [num cns]
      (let [[upd-num upd-cns] (update-tables num cns i cs)]
         (recur (inc i) upd-num upd-cns)))))

(defn issue
  "Given an amount to change and a set of coins, it returns the fewest coins
  such that the sum of their values equals the change."
  [amount coins]
  (let [[_ cns] (build-tables amount coins)]
    (loop [usd []
           rmn amount]
      (if-let [c (get cns rmn)]
        (recur (conj usd c) (- rmn c))
        usd))))
