(ns day3
  (:require [clojure.string :as string]))

(def input (slurp "../input.txt"))

(def sample "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn size [sample]
  [(count sample) (count (first sample))])

(defn parse [sample]
  (let [m (string/split-lines sample)]
    (conj (size m) m)))

(defn getv [sch i j]
  (get (get sch i) j))

(defn n? [ch]
  (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0} ch))

(defn emp? [ch]
  (#{\.} ch))

(defn symb? [ch]
  (not (or (emp? ch) (n? ch) nil)))

(comment
  (symb? \3))

(defn arounds [i j]
  (for [a (range (dec i) (+ i 2))
        b (range (dec j) (+ j 2))
        :when (not= [a b] [i j])]
    [a b]))

(defn adj? [sch i j]
  (->> (arounds i j)
       (keep (fn [[a b]] (getv sch a b)))
       (some symb?)))

(comment
  (symb? \#)
  (symb? \.)
  (adj? [[\1 \2 \3] [\# \. \.]] 0 0)
  (adj? [[\1 \2 \3] [\. \. \.]] 0 0)  
  )


(defn last-col? [sch j]
  (= (count (first sch)) (inc j)))

(defn check-number
  [buf adj sch syms  i j]
  (let [ch (getv sch i j)
        adj (or adj (adj? sch i j))
        last-col? (last-col? sch j)]
    (cond
      (and last-col? adj)
      [[] false sch (conj syms (conj buf ch))]
      (and last-col? (not adj))
      [[] false sch syms]
      (and (not last-col?))
      [(conj buf ch) adj sch syms])))

(comment (check-number [] false [[1]] [] 0 0))

(defn check-non-numb
  [buf adj sch syms i j]
  (cond
    (empty? buf)
    [buf adj sch syms]
    (and (seq buf) adj)
    [[] false sch (conj syms buf)]
    (and (seq buf) (not adj))
    [[] false sch syms]))

(defn check-point
  [[buf adj sch syms] [i j]]
  (if (n? (getv sch i j))
    (check-number buf adj sch syms i j)
    (check-non-numb buf adj sch syms i j)))

(comment
  (def a (check-point [[] false [[\1 \2 \3] [\. \. \.]] []] [0 0]))
  (def b (check-point a [0 1]))
  (def c (check-point b [0 2]))

  (def a1 (check-point [[] false [[\1 \2 \3] [\. \# \.]] []] [0 0]))
  (def b1 (check-point a1 [0 1]))
  (def c1 (check-point b1 [0 2]))
  )

(defn find-valid-zone
  [row-size col-size sch]
  (->> (for [i (range row-size) j (range col-size)] [i j])
       (reduce check-point [[] false sch []])))

(comment
  (find-valid-zone 2 3 [[\1 \2 \3] [\. \# \.]])
  (find-valid-zone 2 3 [[\1 \2 \3] [\. \. \.]])
  (parse sample)
  
  )

(defn make-sum [chars]
  (->> chars
       (map (comp read-string #(apply str %)))
       (apply +)))

(comment
 (apply str [\1 \2 \3])
 (map str [[\1 \2 \3] [\3 \5 \6]])
 (make-sum [[\1 \2 \3] [\3 \5 \6]])
 )

(defn solve [sample]
  (let [[rsize csize sch] (parse sample)]
    (-> (find-valid-zone rsize csize sch))))

(comment
  (solve sample))

  
