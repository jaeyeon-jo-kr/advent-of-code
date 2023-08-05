(ns aoc.matrix_spiral)

(defn matrix
  [n]
  (let [matrix (vec (repeat n (vec (repeat n 0))))]
    (reduce
     (fn [[[x y] dir level matrix] cnt]
       (cond
         (and (= dir :right) (< (+ x level) (dec n)))
         [[(inc x) y] :right level (assoc-in matrix [y x] cnt)]

         (and (= dir :right) (= (+ x level) (dec n)))
         [[x (inc y)] :down level (assoc-in matrix [y x] cnt)]

         (and (= dir :down) (< (+ y level) (dec n)))
         [[x (inc y)] :down level (assoc-in matrix [y x] cnt)]

         (and (= dir :down) (= (+ y level) (dec n)))
         [[(dec x) y] :left level (assoc-in matrix [y x] cnt)]

         (and (= dir :left) (< 0 (- x level)))
         [[(dec x) y] :left level (assoc-in matrix [y x] cnt)]

         (and (= dir :left) (= 0 (- x level)))
         [[x (dec y)] :up (inc level) (assoc-in matrix [y x] cnt)]

         (and (= dir :up) (< 0 (- y level)))
         [[x (dec y)] :up level (assoc-in matrix [y x] cnt)]

         (and (= dir :up) (= 0 (- y level)))
         [[(inc x) y] :right level (assoc-in matrix [y x] cnt)]

         :else
         [[[x y] dir level matrix] cnt]))
     [[0 0] :right 0 matrix] (range 1 (inc (* n n))))))

(comment 
  (matrix 1)
  (matrix 2)
  (matrix 3)
  (matrix 4)
  (matrix 5)
  (range 1 10)
  (vec (repeat 3 (vec (repeat 3 0))))
  )