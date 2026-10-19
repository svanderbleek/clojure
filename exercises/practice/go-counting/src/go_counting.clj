(ns go-counting
  (:require [clojure.set :as set]))

(defn parse-char [c]
  (case c
    \B :black
    \W :white
    :empty))

(defn parse-row [row]
  (mapv parse-char row))

(defn stone-at [board position]
  (get-in board (reverse position)))

(defn nearby [[x y]]
  [[(- x 1) y] [(+ x 1) y] [x (- y 1)] [x (+ y 1)]])

(defn owner [borders]
  (case borders
    #{:black} :black
    #{:white} :white
    nil))

(defn not-empty? [stone]
  (not= stone :empty))

(defn fill-territory [board start]
  (loop [queue [start]
         visited #{}
         borders #{}]
    (if (empty? queue)
      {:stones visited :owner (owner borders)}
      (let [curr (peek queue)
            next (pop queue)
            stone (stone-at board curr)]
        (if (or (not stone)
                (contains? visited curr))
          (recur next visited borders)
          (if (not-empty? stone)
            (recur next visited (conj borders stone))
            (recur (into next (nearby curr))
                   (conj visited curr)
                   borders)))))))

(defn territory [grid start]
  (let [board (mapv parse-row grid)]
    (case (stone-at board start)
      nil (throw (ex-info "Out of bounds" {:pos start}))
      :empty (fill-territory board start)
      {:stones #{} :owner nil})))

(defn all-positions [board]
  (for [x (range 0 (count (get board 0)))
        y (range 0 (count board))]
    [x y]))

(defn find-empty [board]
  (filter #(= :empty (stone-at board %)) (all-positions board)))

(defn found-for [terr]
  (case (:owner terr)
    :black :black-territory
    :white :white-territory
    nil    :null-territory))

(defn territories [grid]
  (let [board (mapv parse-row grid)]
    (loop [empty (set (find-empty board))
           found {:black-territory #{} :white-territory #{} :null-territory #{}}]
      (if (empty? empty)
        found
        (let [curr   (first empty)
              terr   (fill-territory board curr)
              stones (:stones terr)]
          (recur (set/difference empty stones)
                 (update found (found-for terr) set/union stones)))))))
