(ns kata.mixin
  (:require [clojure.test :refer :all]
            [clojure.string :as string]))

(defn filter-lower-case
  [str]
  (filter #(Character/isLowerCase %) str))

(defn remove-one-char
  [m]
  (reduce-kv (fn [m k v]
               (if (< 1 v)
                 (assoc m k v)
                 m)) {} m))

(defn merge-map
  [m1 m2]
  (let [k1 (set (keys m1))
        k2 (set (keys m2))
        k1-k2 (clojure.set/difference k1 k2)
        k2-k1 (clojure.set/difference k2 k1)
        k1+k2 (clojure.set/intersection k1 k2)]
    (concat
     (map (fn [k] [k "1" (m1 k)]) k1-k2)
     (map (fn [k] [k "2" (m2 k)]) k2-k1)
     (map (fn [k]
            (let [l1 (m1 k)
                  l2 (m2 k)]
              (cond
                (< l1 l2) [k "2" (m2 k)]
                (< l2 l1) [k "1" (m1 k)]
                (= l1 l2) [k "=" (m2 k)]))) k1+k2))))


(defn mix
  [s1 s2]
  (->> (list s1 s2)
       (map (comp remove-one-char
                  frequencies
                  filter-lower-case))
       (apply merge-map) 
       (sort-by #(int (first %)))
       (sort-by #(second %))
       (sort-by #(nth % 2) >)
       (map (fn [[ch n cnt]]
              (->> (repeat cnt ch)
                   (apply str)
                   (str n ":"))))
       (clojure.string/join "/")
       ))

(comment 
  (mix "Are they here" "yes, they are here") 
  (filter-lower-case "abc")
  (mix "abcdd" "abcc")
  (Character/isLowerCase \a)
  (frequencies "Are they here")
  (frequencies "yes, they are here")
  
  (->> (sort-by first [[9 1] [0 1] [2 5] [2 4] ])
       (sort-by second))
  )


(deftest a-test1
  (testing "Basic tests"
    (is (= (mix "Are they here" "yes, they are here") 
           "2:eeeee/2:yy/=:hh/=:rr"))
    (is (= (mix "looping is fun but dangerous" "less dangerous than coding")
           
           "1:ooo/1:uuu/2:sss/=:nnn/1:ii/2:aa/2:dd/2:ee/=:gg"))
    (is (= (mix " In many languages" " there's a pair of functions") 
           "1:aaa/1:nnn/1:gg/2:ee/2:ff/2:ii/2:oo/2:rr/2:ss/2:tt"))
    (is (= (mix "Lords of the Fallen" "gamekult") "1:ee/1:ll/1:oo"))
    (is (= (mix "codewars" "codewars") ""))
    (is (= (mix "A generation must confront the looming " "codewarrs") "1:nnnnn/1:ooooo/1:tttt/1:eee/1:gg/1:ii/1:mm/=:rr"))))
