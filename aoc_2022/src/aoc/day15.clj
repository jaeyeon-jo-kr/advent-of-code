(ns aoc.day15
  (:require [aoc.util :as u]
            [clojure.string :as str]))

(comment 
  "
   You feel the ground rumble again as the distress signal leads you to a large network of subterranean tunnels. You don't have time to search them all, but you don't need to: your pack contains a set of deployable sensors that you imagine were originally built to locate lost Elves.

The sensors aren't very powerful, but that's okay; your handheld device indicates that you're close enough to the source of the distress signal to use them. You pull the emergency sensor system out of your pack, hit the big button on top, and the sensors zoom off down the tunnels.

Once a sensor finds a spot it thinks will give it a good reading, it attaches itself to a hard surface and begins monitoring for the nearest signal source beacon. Sensors and beacons always exist at integer coordinates. Each sensor knows its own position and can determine the position of a beacon precisely; however, sensors can only lock on to the one beacon closest to the sensor as measured by the Manhattan distance. (There is never a tie where two beacons are the same distance to a sensor.)

It doesn't take long for the sensors to report back their positions and closest beacons (your puzzle input). For example:
   
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3

So, consider the sensor at 2,18; the closest beacon to it is at -2,15. For the sensor at 9,16, the closest beacon to it is at 10,16.

Drawing sensors as S and beacons as B, the above arrangement of sensors and beacons looks like this:

               1    1    2    2
     0    5    0    5    0    5
 0 ....S.......................
 1 ......................S.....
 2 ...............S............
 3 ................SB..........
 4 ............................
 5 ............................
 6 ............................
 7 ..........S.......S.........
 8 ............................
 9 ............................
10 ....B.......................
11 ..S.........................
12 ............................
13 ............................
14 ..............S.......S.....
15 B...........................
16 ...........SB...............
17 ................S..........B
18 ....S.......................
19 ............................
20 ............S......S........
21 ............................
22 .......................B....
 
 This isn't necessarily a comprehensive map of all beacons in the area, though. Because each sensor only identifies its closest beacon, if a sensor detects a beacon, you know there are no other beacons that close or closer to that sensor. There could still be beacons that just happen to not be the closest beacon to any sensor. Consider the sensor at 8,7:

               1    1    2    2
     0    5    0    5    0    5
-2 ..........#.................
-1 .........###................
 0 ....S...#####...............
 1 .......#######........S.....
 2 ......#########S............
 3 .....###########SB..........
 4 ....#############...........
 5 ...###############..........
 6 ..#################.........
 7 .#########S#######S#........
 8 ..#################.........
 9 ...###############..........
10 ....B############...........
11 ..S..###########............
12 ......#########.............
13 .......#######..............
14 ........#####.S.......S.....
15 B........###................
16 ..........#SB...............
17 ................S..........B
18 ....S.......................
19 ............................
20 ............S......S........
21 ............................
22 .......................B....
 
 This sensor's closest beacon is at 2,10, and so you know there are no beacons that close or closer (in any positions marked #).

None of the detected beacons seem to be producing the distress signal, so you'll need to work out where the distress beacon is by working out where it isn't. For now, keep things simple by counting the positions where a beacon cannot possibly be along just a single row.

So, suppose you have an arrangement of beacons and sensors like in the example above and, just in the row where y=10, you'd like to count the number of positions a beacon cannot possibly exist. The coverage from all sensors near that row looks like this:
 
                 1    1    2    2
       0    5    0    5    0    5
 9 ...#########################...
10 ..####B######################..
11 .###S#############.###########.

In this example, in the row where y=10, there are 26 positions where a beacon cannot be present.

Consult the report from the sensors you just deployed. In the row where y=2000000, how many positions cannot contain a beacon?
   ")

;;Sensor at x=8, y=7: closest beacon is at x=2, y=10

(defn radius
  [sx sy cx cy]
  (+ (abs (- cx sx))
     (abs (- cy sy))))

(defn remain-x
  [radius y-move]
  (when (<= y-move radius)
    (- radius y-move)))

(defn y-move 
  [sy y]
  (abs (- sy y)))


(defn excluded-x-line
  [sx sy cx cy y]
  (when-let [x (remain-x (radius sx sy cx cy) (y-move sy y))]
    [(- sx x) (+ sx x)]))

(defn exclude-beacon
  [bx [x-min x-max]]
  (cond
    (= bx x-min) [[(inc x-min) x-max]]
    (= bx x-max) [[x-min (dec x-max)]]
    (< x-min bx x-max) [[x-min (dec bx)] [(inc bx) x-max]]
    :else [[x-min x-max]]))


(defn exclude-beacons
  [bx-coll x-lines]
  (reduce (fn [x-lines bx]
            (mapcat #(exclude-beacon bx %) x-lines))
          x-lines bx-coll))

(defn mergeable-lines?
  [[_ ax-max] [bx-min _]]
  (and ax-max
       bx-min
       (<= bx-min ax-max)))

(defn merge-line
  [[ax-min ax-max] [_ bx-max]]
  [ax-min (max ax-max bx-max)])


(defn dedupe-all-lines
  [x-lines] 
  (->> (sort-by first x-lines)
       (reduce
        (fn [lines new-line]
          (if (mergeable-lines? (last lines) new-line)
            (update lines (dec (count lines)) #(merge-line % new-line))
            (conj lines new-line)))
        [])))


(defn beacon-x-list
  [sensor-info y]
  (keep (fn [[_ _ bx by]] (when (= y by) bx)) sensor-info))

(defn parse-line
  "parse line such as 
   Sensor at x=3844106, y=3888618: closest beacon is at x=3225436, y=4052707"
  [line]
  (map read-string (re-seq #"\d+" line)))

(defn parse-sensor-info
  [text]
  (map parse-line
       (str/split-lines text)))

(defn find-excluded-x-lines
  [sensor-list y]
  (keep (fn [[sx sy cx cy]]
          (excluded-x-line sx sy cx cy y)) sensor-list))

(defn distance-of-lines
  [lines]
  (apply +
         (map (fn [[a b]] (inc (- b a))) lines)))

(defn solve-file
  [filename y]
  (let [sensor-info (parse-sensor-info (slurp filename))
        x-lines (find-excluded-x-lines sensor-info y)
        
        excluded-beacons
        (exclude-beacons (beacon-x-list sensor-info y) x-lines)

        deduped
        (dedupe-all-lines excluded-beacons)]
    (println deduped)
    (distance-of-lines deduped)))  

(defn line-sub
  [[x1-min x1-max] [x2-min x2-max]]
  (cond
    (<= x1-min x2-min x1-max x2-max)
    [[x1-min (dec x2-min)]]

    (<= x2-min x1-min x2-max x1-max)
    [[(inc x2-max) x1-max]]

    (<= x1-min x2-min x2-max x1-max)
    [[x1-min (dec x2-min)] [(inc x2-max) x1-max]]))

(defn all-scanned-lines
  [sensor-info y]
  (dedupe-all-lines
   (find-excluded-x-lines sensor-info y)))

(defn find-tuning-frequency
  [sensor-info]
  (->> (map #(all-scanned-lines sensor-info %) (range 0 1000))
       (map-indexed (fn [i sequence]
                      [i sequence]))
       (filter (fn [[_ lines]]
                 (and (= 2 (count lines))
                      (= (- (first (second lines))
                            (second (first lines))) 2))))))



(comment

  (dedupe-all-lines
   (find-excluded-x-lines sensor-info 10))

  (dedupe-all-lines
   (find-excluded-x-lines sensor-info 12))


  (def sensor-info
    (parse-sensor-info (slurp "day15_sample.txt")))
  
  (find-tuning-frequency sensor-info)

  (def sensor-info-15 (parse-sensor-info (slurp "day15_input.txt")))

  (find-tuning-frequency sensor-info-15)

  (all-scanned-lines sensor-info 10)
  (all-scanned-lines sensor-info 8)

  (dedupe-all-lines (find-excluded-x-lines sensor-info 0))
  (dedupe-all-lines (find-excluded-x-lines sensor-info 1))
  (dedupe-all-lines (find-excluded-x-lines sensor-info 2))
  (dedupe-all-lines (find-excluded-x-lines sensor-info 3))
  (dedupe-all-lines (find-excluded-x-lines sensor-info 4))
  (dedupe-all-lines (find-excluded-x-lines sensor-info 5))
  (dedupe-all-lines (find-excluded-x-lines sensor-info 6))
  (dedupe-all-lines (find-excluded-x-lines sensor-info 11))

  (line-sub [0 50] [1 2])

  (dedupe-all-lines (find-excluded-x-lines sensor-info 1))

  (dedupe-all-lines line)
  (quot (radius 0 0 0 2) 2)
  (excluded-x-line 0 0 0 2 0)
  (out-of-x-line 0 0 0 2 0)
  (out-of-x-line 0 0 0 2 -1)
  (out-of-x-line 0 0 0 2 -2)



  (count [0 1])
  (sort-by first
           '([10412 2750293] [-9867 10410] [1366733 1984817] [1889069 3367619] [-10379 10410] [2535371 3715829] [10412 32751] [3445903 4253073] [3082762 4908902]))
  (dedupe-all-lines
   '([10412 2750293] [-9867 10410] [1366733 1984817] [1889069 3367619] [-10379 10410] [2535371 3715829] [10412 32751] [3445903 4253073] [3082762 4908902]))
  (dedupe-lines [[1 5] [3 8] [7 9]])
  (dedupe '(1 2 8 3 4 5 8 8))
  (->> (mapcat #(exclude-beacon 1 %) [[-10 10] [0 5]])
       (mapcat #(exclude-beacon 2 %)))
  (exclude-beacons [1 2 3 4]
                   [[-10 10] [0 5]])

  (exclude-beacons []
                   [[-10 10] [0 5]])
  )



