(ns knapsack
  (:require [clojure.core.match   :refer [match]]))

(defn max-v [i w items]
  (match [i w]
         [0 _] 0
         [i w] (let [{:keys [weight value]} (get items (dec i))]
                (if (< w weight)
                        (max-v (dec i) w items)
                        (max (max-v (dec i) w items)
                             (+ (max-v (dec i) (- w weight) items) value))))))

(defn maximum-value
  "Calculates the maximum value that can be packed."
  [max-wt items]
  (max-v (count items) max-wt items))
