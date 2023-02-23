(ns aoc.day6)

(defn marker?
  ([text size]
   (= size
      (count text)
      (count (set text))))
  ([text]
   (marker? text 4)))

(defn next-value
  ([text ch size]
   (if (= (count text) size)
     (concat (rest text) (list ch))
     (str text ch)))
  ([text ch]
   (next-value text ch 4)))

(defn find-first-marker
  ([text size]
   (reduce (fn [[text cnt] ch]
             (if (marker? text size)
               (reduced cnt)
               [(next-value text ch size) (inc cnt)]))
           ["" 0] text))
  ([text]
   (reduce (fn [[text cnt] ch]
             (if (marker? text)
               (reduced cnt)
               [(next-value text ch) (inc cnt)]))
           ["" 0] text)))

(defn find-first-marker-2
  [text]
  (find-first-marker text 14))



(comment
  (marker? "")

  (find-first-marker "bvwbjplbgvbhsrlpgdmjqwftvncz")
  (= 4 1 4)
  (split-at 4 "dasdfg")

  (count "abcc")
  (set "abc"))