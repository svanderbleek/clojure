(ns sieve)

(defn sieve [num]
  (let [nums (range 2 (inc num))]
    (loop [prime (first nums)
           primes []
           remain (rest nums)]
      (if (not prime)
        primes
        (if (empty? remain)
          (conj primes prime)
          (let [unmarked (filter #(pos? (mod % prime)) remain)]
            (recur (first unmarked) (conj primes prime) (rest unmarked))))))))
