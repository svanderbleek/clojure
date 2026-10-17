(ns etl
  (:require [clojure.string :as str]))

(defn flip [k vs]
  (zipmap (map str/lower-case vs)
          (repeat k)))

(defn transform [source]
  (reduce-kv (fn [target k vs] (merge target (flip k vs)))
             {}
             source))

(transform {1 ["A"]})
