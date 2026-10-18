(ns run-length-encoding)

(defn- encode [run]
  (let [n (count run)
        c (first run)]
    (if (= n 1)
      c
      (str n c))))

(defn run-length-encode
  "Encodes a string with run-length encoding."
  [plaintext]
  (->> plaintext
       (partition-by identity)
       (map encode)
       (apply str)))

(def ciphertext "12WB12W3B24WB")

(defn- ->int [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException _
      1)))

(defn- decode [n c]
  (repeat n c))

(defn run-length-decode
  "Decodes a run-length-encoded string."
  [ciphertext]
  (reduce (fn [s [_ n c]]
            (apply (partial str s) (decode (->int n) c)))
          ""
          (re-seq #"(\d*)(\D)" ciphertext)))

(run-length-decode ciphertext)
