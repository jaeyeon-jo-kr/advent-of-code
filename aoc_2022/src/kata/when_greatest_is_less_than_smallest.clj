(ns kata.when-greatest-is-less-than-smallest)

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcd [a b]
  (let [gcd (gcd a b)]
    (* gcd
       (quot a gcd)
       (quot b gcd))))


(defn greatest [x y n]
  ;; Good Luck!
  (let [lcd (lcd x y)
        from (- n (mod n lcd))]
    (if (= from n)
      0
      from)))

(defn smallest [x y n]
  ;; Good Luck!
  (let [lcd (lcd x y)
        from (- n (mod n lcd))]
    (+ lcd from)))



(comment
  (-> (iterate inc 10) first)
  (->> (range 1 11)
       (reverse)
       (drop-while #(not (and (zero? (mod % 2))
                              (zero? (mod % 3)))))
       first
       )
  (gcd 10 100)
  (greatest 10 100 100)
  (greatest 2 3 20)
  (smallest 2 3 20)
  (smallest 1 2 3)
  (greatest 1000000007 1000000009 10000000000000000000)
  (smallest 1000000007 1000000009 10000000000000000000)
  (* 4 6)
  (gcd 4 6)
  (lcd 4 6)
  (lcd 10 18)
  2 * 2 
  2 * 3
  )