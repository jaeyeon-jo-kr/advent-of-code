(ns aoc.util)

(defn rcomp
  "Reverse composition"
  [& funcs]
  (apply comp
         (reverse funcs)))

(comment 
  ((rcomp #(* 2 %) #(+ % 3)) 10)
  )