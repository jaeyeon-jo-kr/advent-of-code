(ns aoc.test-day2-rpc
  (:require [aoc.day2-rpc :refer :all]
            [clojure.test :refer :all]))

(deftest selected-point
  (testing "shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors)"
    (is (= 1 (point :X)))
    (is (= 2 (point :Y)))
    (is (= 3 (point :Z)))))

(deftest outcome-round
  (testing "outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won)"
    (is (= 0 (outcome :lost)))
    (is (= 3 (outcome :draw)))
    (is (= 6 (outcome :won)))))

(deftest winning
  (testing "Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock. If both players choose the same shape, the round instead ends in a draw."
    (is (= (result :A :Z) :lost))
    (is (= (result :C :Y) :lost))
    (is (= (result :B :X) :lost))

    (is (= (result :A :X) :draw))
    (is (= (result :B :Y) :draw))
    (is (= (result :C :Z) :draw))

    (is (= (result :A :Y) :won))
    (is (= (result :B :Z) :won))
    (is (= (result :C :X) :won))))

(deftest first-column
  (testing "A for Rock, B for Paper, and C for Scissors."
    (is (= (->item "A") :A))
    (is (= (->item "B") :B))
    (is (= (->item "C") :C))))

(deftest second-column
  (testing "X for Rock, Y for Paper, and Z for Scissors."
    (is (= (->item "X") :X))
    (is (= (->item "Y") :Y))
    (is (= (->item "Z") :Z))))

(deftest parse-input-test
  (testing "parse input"
    (is (= (parse "A Y\nB X\nC Z")
           '((:A :Y)
             (:B :X)
             (:C :Z))))))


(deftest rounds-test
  (testing "In the first round, your opponent will choose Rock (A), and you should choose Paper (Y). This ends in a win for you with a score of 8 (2 because you chose Paper + 6 because you won)."
    (is (= (round :A :Y) 8)))
  (testing "In the second round, your opponent will choose Paper (B), and you should choose Rock (X). This ends in a loss for you with a score of 1 (1 + 0)"
    (is (= (round :B :X) 1)))
  (testing "The third round is a draw with both players choosing Scissors, giving you a score of 3 + 3 = 6."
    (is (= (round :C :Z) 6))))

(deftest total-round-test
  (testing "In this example, if you were to follow the strategy guide, you would get a total score of 15 (8 + 1 + 6)"
    (is (= (total-score 
            [[:A :Y]
             [:B :X]
             [:C :Z]]) 15))))

(deftest solve-day2-part1
  (testing "solve part1"
    (is (solve-part1 "day2_input.txt"))))

(deftest round-test
  (testing "Anyway, the second column says how the round needs to end: X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win. Good luck!"
    (is (= (round-point2 :A :X) (+ 3 0)))
    (is (= (round-point2 :A :Y) (+ 1 3)))
    (is (= (round-point2 :A :Z) (+ 2 6)))

    (is (= (round-point2 :B :X) (+ 1 0)))
    (is (= (round-point2 :B :Y) (+ 2 3)))
    (is (= (round-point2 :B :Z) (+ 3 6)))

    (is (= (round-point2 :C :X) (+ 2 0)))
    (is (= (round-point2 :C :Y) (+ 3 3)))
    (is (= (round-point2 :C :Z) (+ 1 6)))))


(deftest solve-day2-part1
  (testing "solve part1"
    (is (solve-part2 "day2_input.txt"))))


