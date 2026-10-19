(ns pascals-triangle)

(defn row-val [prev n]
  (condp = n
    1 1
    (inc (count prev)) 1
    (+ (get prev (dec (dec n))) (get prev (dec n)))))

(row-val [1 1] 3)

(defn row [prev]
  (let [size (inc (count prev))]
    (loop [n size
           r []]
      (if (zero? n)
        r
        (recur (dec n) (conj r (row-val prev n)))))))

(def triangle (iterate row [1]))
