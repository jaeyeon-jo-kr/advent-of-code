(ns aoc.day11 
  (:require [clojure.string :as s]))

(def inspect-time (atom []))

(defn register-starting-items
  [item content]
  (assoc item :items 
          (->> (s/split content #",")
               (map read-string)
               vec)))

(defn create-fn
  [left oper right]
  (println "print create-fn" left oper right)
  (->> ["(fn [old] (" oper " " left " " right "))"]
       (apply str)
       read-string
       eval))


(defn register-operation
  [monkey content]
  (->> (s/split (s/trim content) #" ")
       nnext
       (apply create-fn)
       (assoc monkey :oper-fn)))


(defn register-division 
  [monkey content]
  (assoc monkey :division
         (-> (s/replace content #" divisible by " "")
             read-string)))


(defn register-if-true
  [monkey content]
  (assoc monkey :if-true 
         (-> (s/replace content #" throw to monkey " "")
             read-string)))

(defn register-if-false
  [monkey content]
  (assoc monkey :if-false
         (-> (s/replace content #" throw to monkey " "")
             read-string)))


(defn property
  [monkey name content]
  (case name
    "Starting items" (register-starting-items monkey content)
    "Operation" (register-operation monkey content)
    "Test" (register-division monkey content)
    "If true" (register-if-true monkey content)
    "If false" (register-if-false monkey content)
    monkey))

(defn monkey-number
  [name]
  (-> (s/split name #" ")
      second
      read-string))

(defn register-monkey
  [monkeys [name content]]
  (if (s/includes? name "Monkey")
    (conj monkeys {:number (monkey-number name)
                   :pass []
                   :received []})
    (let [last-num (dec (count monkeys))]
      (update monkeys last-num property name content))))


(defn parse
  [contents]
  (->> contents
       (s/split-lines)
       (map (comp #(s/split % #":") s/trim))
       (reduce register-monkey (vector))))

(defn increase-worry
  [item oper-fn] 
  (-> (oper-fn item)
      (doto (->> (println "\tWorry level is to ")))))

(defn decrease-worry
  [item]
  (-> (quot item 3)
      (doto (->> 
             (println "\tMonkey gets bored with item."
                      "Worry level is divided by " 3 " to ")))))

(defn pass-to-monkey
  [item {:keys [if-true if-false division]}] 
  (if (zero? (rem item division))
    (do 
      (println "\tCurrent worry level is divisible by " division)
      (println "\tItem with worry level "
               item
               " is thrown to monkey " if-true ".")
      if-true)
    (do 
      (println "\tCurrent worry level is not divisible by " division)
      (println "\tItem with worry level "
               item
               " is thrown to monkey " if-false ".")
      if-false)))

(defn inspect-monkey
  [monkey]
  (if-let [item (peek (:items monkey))]
    (do (println "Monkey inspects an item with a worry level of " item)
        (let [worried (-> item
                          (increase-worry (:oper-fn monkey))
                          (decrease-worry))]

          (-> monkey
              (update :pass
                      #(conj % [(pass-to-monkey worried monkey) worried]))
              (update :items pop)
              inspect-monkey)))
    monkey))

(defn pass-items
  [monkeys [pass-to item]]
  
  (update-in monkeys
             [pass-to :items]
             #(conj % item)))

(defn pass-all-items
  [monkeys monkey]
  (->> (:pass monkey)
       (reduce pass-items monkeys)
       (mapv #(assoc % :pass (vector)))))

(defn play-round
  [monkeys]
  (->> (range 0 (count monkeys))
       (reduce (fn [monkeys i]
                 (println "\nMonkey " i ":")
                 (->> (update monkeys i inspect-monkey)
                      ((juxt identity #(nth % i)))
                      (apply pass-all-items))) monkeys)))

(comment 
  (-> (slurp "day11_sample.txt")
      parse
      play-round)
  (mapcat :list [{:list ["b" "a" "c"]}
                 {:list ["b" "a" "c"]}])
  
  0
  (parse "Monkey 0:
                         Starting items: 79, 98
                         Operation: new = old * 19
                         Test: divisible by 23
                           If true: throw to monkey 2
                           If false: throw to monkey 3
                          
                          Monkey 1:
                         Starting items: 54, 65, 75, 74
                         Operation: new = old + 6
                         Test: divisible by 19
                           If true: throw to monkey 2
                           If false: throw to monkey 0
                       ")
  (->> "Monkey 0:
                Starting items: 79, 98
                Operation: new = old * 19
                Test: divisible by 23
                  If true: throw to monkey 2
                  If false: throw to monkey 3
                 
                 Monkey 1:
                Starting items: 54, 65, 75, 74
                Operation: new = old + 6
                Test: divisible by 19
                  If true: throw to monkey 2
                  If false: throw to monkey 0
              "
       
       (s/split-lines)
       (map s/trim)
       (map (fn [s] (s/split s #":")))
       )
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
   
   Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0
"
  
  )