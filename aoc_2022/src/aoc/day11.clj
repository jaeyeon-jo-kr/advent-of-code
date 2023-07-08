(ns aoc.day11 
  (:require [clojure.string :as s]))

(def inspect
  (atom {:round 0
         :inspect []}))

(defn reset-inspect! []
  (swap! inspect {:round 0 :inspect []}))

(defn update-inpect-time!
  [monkey-number]
  (swap! inspect #(update-in  % [:inspect monkey-number] (fnil inc 0))))

(defn update-round!
  []
  (swap! inspect #(update % :round inc)))

(defn register-starting-items
  [item content]
  (assoc item :items 
          (->> (s/split content #",")
               (map (comp biginteger read-string))
               vec)))

(defn create-fn
  [left oper right]
  #_(println "print create-fn" left oper right)
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

(defn set-monkey-reduce-division
  [monkeys]
  (let [reduce-div
        (->> monkeys
             (map :division)
             (apply *))]
    (mapv #(assoc % :reduce-div reduce-div) monkeys)))

(defn parse
  [contents]
  (->> contents
       (s/split-lines)
       (map (comp #(s/split % #":") s/trim))
       (reduce register-monkey (vector))
       set-monkey-reduce-division))

(defn increase-worry
  [item oper-fn] 
  (-> (oper-fn item)
    #_  (doto (->> (println "\tWorry level is to ")))))

(defn decrease-worry
  [item reduce-div]
  (-> (mod item reduce-div)
      (doto (->> 
           #_  (println "\tMonkey gets bored with item."
                      "Worry level is divided by " 3 " to ")))))

(defn pass-to-monkey
  [item {:keys [if-true if-false division]}] 
  (if (zero? (rem item division))
    (do 
   #_   (println "\tCurrent worry level is divisible by " division)
    #_  (println "\tItem with worry level "
               item
               " is thrown to monkey " if-true ".")
      if-true)
    (do 
      #_(println "\tCurrent worry level is not divisible by " division)
      #_(println "\tItem with worry level "
               item
               " is thrown to monkey " if-false ".")
      if-false)))


(defn inspect-monkey
  [monkey]
  (if-let [item (peek (:items monkey))]
    (do 
      (update-inpect-time! (:number monkey))
   #_   (println "Monkey inspects an item with a worry level of " item)
      (let [worried (-> item
                        (increase-worry (:oper-fn monkey))
                        (decrease-worry (:reduce-div monkey))
                        )]

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
           #_      (println "\nMonkey " i ":")
                 (->> (update monkeys i inspect-monkey)
                      ((juxt identity #(nth % i)))
                      (apply pass-all-items))) monkeys)))

(defn play-round-20
  [monkeys]
  (reset-inspect!)
  (->> (iterate play-round monkeys)
       (take 21)))

(defn play-round-10000
  [monkeys]
  (reset-inspect!)
  (->> (range 10000)
       (reduce (fn [monkeys round]
                 (println "round : " round)
                 (play-round monkeys))
               monkeys)))

(comment 
  (BigInteger. 100)
  
  (-> (slurp "day11_sample.txt")
      parse
      play-round)
  (-> (slurp "day11_sample.txt")
      parse
      play-round-20)
  (-> (slurp "day11_input.txt")
      parse
      play-round-20)
  (-> (slurp "day11_input.txt")
      parse
      play-round-10000)
  (-> (slurp "day11_sample.txt")
      parse
      play-round-10000)
  (-> (slurp "day11_input.txt")
      parse
      play-round-10000)
  (->> @inspect
       :inspect
       vals
       (sort >)
       (take 2)
       (apply *))
  
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