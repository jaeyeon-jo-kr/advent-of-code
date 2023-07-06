(ns aoc.day10 
  (:require [clojure.string :as string]))

(comment 
  "You avoid the ropes, plunge into the river, and swim to shore.

The Elves yell something about meeting back up with them upriver, but the river is too loud to tell exactly what they're saying. They finish crossing the bridge and disappear from view.

Situations like this must be why the Elves prioritized getting the communication system on your handheld device working. You pull it out of your pack, but the amount of water slowly draining from a big crack in its screen tells you it probably won't be of much immediate use.

Unless, that is, you can design a replacement for the device's video system! It seems to be some kind of cathode-ray tube screen and simple CPU that are both driven by a precise clock circuit. The clock circuit ticks at a constant rate; each tick is called a cycle.

Start by figuring out the signal being sent by the CPU. The CPU has a single register, X, which starts with the value 1. It supports only two instructions:

addx V takes two cycles to complete. After two cycles, the X register is increased by the value V. (V can be negative.)
noop takes one cycle to complete. It has no other effect.
The CPU uses these instructions in a program (your puzzle input) to, somehow, tell the screen what to draw.

Consider the following small program:
noop
addx 3
addx -5
Execution of this program proceeds as follows:

At the start of the first cycle, the noop instruction begins execution. During the first cycle, X is 1. After the first cycle, the noop instruction finishes execution, doing nothing.
At the start of the second cycle, the addx 3 instruction begins execution. During the second cycle, X is still 1.
During the third cycle, X is still 1. After the third cycle, the addx 3 instruction finishes execution, setting X to 4.
At the start of the fourth cycle, the addx -5 instruction begins execution. During the fourth cycle, X is still 4.
During the fifth cycle, X is still 4. After the fifth cycle, the addx -5 instruction finishes execution, setting X to -1.
Maybe you can learn something by looking at the value of the X register throughout execution. For now, consider the signal strength (the cycle number multiplied by the value of the X register) during the 20th cycle and every 40 cycles after that (that is, during the 20th, 60th, 100th, 140th, 180th, and 220th cycles).

For example, consider this larger program:
   

The interesting signal strengths can be determined as follows:

During the 20th cycle, register X has the value 21, so the signal strength is 20 * 21 = 420. (The 20th cycle occurs in the middle of the second addx -1, so the value of register X is the starting value, 1, plus all of the other addx values up to that point: 1 + 15 - 11 + 6 - 3 + 5 - 1 - 8 + 13 + 4 = 21.)
During the 60th cycle, register X has the value 19, so the signal strength is 60 * 19 = 1140.
During the 100th cycle, register X has the value 18, so the signal strength is 100 * 18 = 1800.
During the 140th cycle, register X has the value 21, so the signal strength is 140 * 21 = 2940.
During the 180th cycle, register X has the value 16, so the signal strength is 180 * 16 = 2880.
During the 220th cycle, register X has the value 18, so the signal strength is 220 * 18 = 3960.
The sum of these signal strengths is 13140.

Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles. What is the sum of these six signal strengths?

"
  )

(def status 
  {:cycle 0 
   :value 1
   :sprite [\# \# \#]
   :crt [[] [] [] [] [] []]})

(defn update-sprite
  [{:keys [value] :as status}]
  (assoc status :sprite
         (-> (repeat (dec value) \.)
             (concat [\# \# \#])
             (doto (->> (println "current sprite : "))))))

(defn row-num
  [crt]
  (reduce (fn [num row]
            (if (= 40 (count row))
              (inc num)
              (reduced num))) 0 crt))

(defn update-row
  [row sprite]
  (let [col-num (count row)]
    (println "current crt col : " col-num)
    (-> (conj row (nth sprite col-num \.))
        (doto (->> (apply str) 
                   (println "current crt row : "))))))

(defn update-crt
  [{:keys [crt sprite] :as status}]
  (let [n (row-num crt)]
    (println "current crt row : " n)
    (update-in status
               [:crt n]
               #(update-row % sprite))))

(def signals-sum (atom 0))

(defn update-signals-sum!
  [{:keys [cycle value]}]
  (when (#{20 60 100 140 180 220} cycle) 
    (swap! signals-sum #(-> (* cycle value)
                            (+ %)))))

(defn parse-line
  [line]
  (let [[command num] (string/split line #" ")]
    (case command
      "noop" [:noop]
      "addx" [:addx (read-string num)])))

(defn inst-cycle
  [status inst-fn]
  (-> status
      (update :cycle inc)
      (doto (->> :cycle (println "current cycle : ")))
      (update-crt)
      (update :value inst-fn)
      (doto (->> :value (println "updated value : ")))
      (update-sprite)
      (doto (-> :ff (println "end cycle\n")))))

(defn instruct
  [register [cmd num]] 
  (case cmd
    :noop (inst-cycle register identity)
    :addx (-> register
              (inst-cycle identity)
              (inst-cycle #(+ % num)))))

(defn parse
  [content]
  (->> (string/split-lines content)
       (map parse-line)))

(defn format-crt
  [crt-content]
  (->> crt-content
       (map (partial apply str))
       (interpose "\n")
       (apply str)))


(comment 
  (->> (slurp "day10_sample.txt")
      parse
      (reduce instruct status)
       :crt
       format-crt)
  
  "##..##..##..##..##..##..##..##..##..##..
   ###...###...###...###...###...###...###.
   ####....####....####....####....####....
   #####.....#####.....#####.....#####.....
   ######......######......######......####
   #######.......#######.......#######....."
  
    (->> (slurp "day10_input.txt")
       parse
       (reduce instruct status)
       :crt
       format-crt)
  
"###..###..####..##..###...##..####..##..
 ####.#..#....#.#..#.#..#.#..#....#.#..#.
 ####.###....#..#....#..#.#..#...#..#..#.
 ###..#..#..#...#.##.###..####..#...####.
 ##...#..#.#....#..#.#.#..#..#.#....#..#.
 ###..###..####..###.#..#.#..#.####.#..#."
  "PBZGRAZA"

  )