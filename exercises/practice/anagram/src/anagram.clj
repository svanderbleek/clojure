(ns anagram
  (:require [clojure.string :as str]))

(defn anagram? [word1 word2]
  (let [lw1 (str/lower-case word1)
        lw2 (str/lower-case word2)]
    (and (= (frequencies lw1) (frequencies lw2))
         (not= lw1 lw2))))

(defn anagrams-for
  "Returns all words from candidates that are anagrams of the given word."
  [word candidates]
  (filter #(anagram? word %) candidates)
)
