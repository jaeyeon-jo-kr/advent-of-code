(ns kata.sum)

(defn abs [n]
  (if (neg? n)
    (- n)
    n))

(defn prime-factors
  [n]
  (let [n (abs n)]
    (->> (range 2 (inc n))
         (reduce (fn [prev a]
                   (if (and (zero? (mod n a))
                            (not (some #(zero? (mod a %)) prev)))
                     (cons a prev)
                     prev))
                 (list))))
  )

(defn sum-of-divided [lst]
  (->> (reduce (fn [m n]
                 (->> (prime-factors n)
                      (reduce (fn [m k]
                                (update m k (fnil + 0) n)) m))) {} lst)
       (into [])
       (sort-by first)))
(comment
  (sum-of-divided [12 15])
  (prime-factors -45)
  (sum-of-divided [15, 30, -45])

  )