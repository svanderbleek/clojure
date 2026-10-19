(ns game-of-life
  (:require [clojure.set :as set]
            [clojure.pprint :as pp])
  (:gen-class))

(defn near [[r c :as pos]]
  (->> (for [dr (range -1 2)
             dc (range -1 2)]
         #{[(+ r dr) (+ c dc)]})
       (apply set/union)
       (#(disj % pos))))

(defn alive? [cell]
  (= 1 cell))

(defn enough? [life]
  (<= 2 life 3))

(defn spawn? [life]
  (= 3 life))

(def live 1)

(def die 0)

(defn rule [cell life]
  (if (or (and (alive? cell) (enough? life))
          (spawn? life))
    live
    die))

(defn find-life [cells pos]
  (->> (near pos)
       (map (fn [neb] (get-in cells neb)))
       (remove nil?)
       (reduce +)))

(defn evolve [cells cell pos]
  (rule cell (find-life cells pos)))

(defn tick [cells]
  (mapv (fn [r row]
          (mapv (fn [c cell]
                  (evolve cells cell [r c]))
                (range)
                row))
        (range)
        cells))

(defn gen-cells [r c]
  (->> (repeatedly #(rand-int 2))
       (take (* r c))
       (partitionv c)
       vec))

(defn tick-until [cells limit]
  (take limit (iterate tick cells)))

(defn run-sleep []
  (doseq [t (tick-until (gen-cells 8 8) 10)]
    (pp/pprint t)
    (Thread/sleep 1000)))

;; can run with clj -M game-of-life
(defn -main [& _] (run-sleep))
