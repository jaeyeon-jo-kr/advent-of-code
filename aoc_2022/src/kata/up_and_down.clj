(ns kata.up-and-down 
  (:require [clojure.string :as string]))

(def next-op
  {>= <=
   <= >=})

(defn arrange
  [string]
  (->> (clojure.string/split string #" ")
       (reduce (fn [[coll op] word]
                 (println (first coll) op word)
                 (if (nil? op)
                   [(cons word coll) <=]
                   (if (op (count (first coll))
                           (count word))
                     [(cons word coll) (next-op op)]
                     [(->> (rest coll)
                           (cons word)
                           (cons (first coll)))
                      (next-op op)]))) ['() nil])
       first
       reverse
       (map-indexed (fn [i word]
                      (if (zero? (mod i 2))
                        (clojure.string/lower-case word)
                        (clojure.string/upper-case word))))
       (clojure.string/join " ")))

(deftest a-test1
  (testing "arrange"

    (test-assert (arrange "who hit retaining The That a we taken")  
                 "who RETAINING hit THAT a THE we TAKEN") ; 3
    (test-assert (arrange "on I came up were so grandmothers")
                 "i CAME on WERE up GRANDMOTHERS so") ; 4
    (test-assert (arrange "way the my wall them him") 
                 "way THE my WALL him THEM") ; 1
    (test-assert (arrange "turn know great-aunts aunt look A to back")
                 "turn GREAT-AUNTS know AUNT a LOOK to BACK") ; 2
    ))
 
