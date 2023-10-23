(ns aoc.weave)

(defn weave
  [q1 q2]
  (->> (map #(vector %1 %2) q1 q2)
       (mapcat identity)))

(comment
  
  (weave '(1 2 3) '("a" "b" "c"))
  )
