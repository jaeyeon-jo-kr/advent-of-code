(ns aoc.test-day9
  (:require [aoc.day9 :refer :all]
            [clojure.test :refer :all]))

(deftest initial-position-test
  (testing "initial position test"
    (is (= initial [0 0]))
    (is (= initial-position 
           {:H [0 0] :T [0 0]}))))

(deftest touched-test
  (testing "is touched?"
    (is (not 
         (touched? {:H [9 9]
                    :T [8 0]})))
    (is (touched? {:H [2 3]
                   :T [2 3]}))))

(deftest move-header-test
  (testing "If the head is ever two steps directly up, down, left, or right from the tail, the tail must also move one step in that direction so it remains close enough:"
    (is (= (tail-step {:H [-5 0]
                       :T [-3 0]})
           {:H [-5 0]
            :T [-4 0]}))
    (is (= (tail-step {:H [0 -5]
                       :T [0 0]})
           {:H [0 -5]
            :T [0 -1]}))))

(deftest parse-line-test
  (testing "R 4"
    (is (= (parse-line "R 4")
           [:R 4]))
    (is (= (parse-line "U 4")
           [:U 4]))
    (is (= (parse-line "L 4")
           [:L 4]))
    (is (= (parse-line "D 4")
           [:D 4]))))

(deftest head-step-test
  (testing "R 4"
    (is (= (head-step {:H [0 0]
                       :T [0 0]} :U)
           {:H [0 1]
            :T [0 0]}))
           
    (is (= (head-step {:H [0 0]
                       :T [0 0]} :D)
           {:H [0 -1]
            :T [0 0]}))
    (is (= (head-step {:H [0 0]
                       :T [0 0]} :R)
           {:H [1 0]
            :T [0 0]}))
    (is (= (head-step {:H [0 0]
                       :T [0 0]} :L)
           {:H [-1 0]
            :T [0 0]}))))

(deftest multi-step-test
  (testing "multi step test"
    (is (= (multi-step
            {:H [0 0]
             :T [0 0]}
            [:L 1])
           {:H [-1 0]
            :T [0 0]})))
    
    
  (testing "R 4
......
......
......
......
H.....  (H covers T, s)

......
......
......
......
s..TH.
"
    (is (= (multi-step
            {:H [0 0]
             :T [0 0]}
            [:R 4])
           {:H [4 0]
            :T [3 0]})))
  (testing "testing u4
......
......
......
......
s..TH.
            
....H.
....T.
......
......
s....." 
    (is (= (multi-step
            {:H [4 0]
             :T [3 0]}
            [:U 4])
           {:H [4 4]
            :T [4 3]})))
    
  (testing "== L 3 ==
....H.
....T.
......
......
s.....
            
.HT...
......
......
......
s.....
"
    (is (= (multi-step
            {:H [4 4]
             :T [4 3]}
            [:L 3])
           {:H [1 4]
            :T [2 4]})))
  
  (testing "== D 1 ==
.HT...
......
......
......
s.....
            
..T...
.H....
......
......
s.....
"
    (is (= (multi-step
            {:H [1 4]
             :T [2 4]}
            [:D 1])
           {:H [1 3]
            :T [2 4]})))
  
  (testing "== R 4 ==
..T...
.H....
......
......
s.....
            
......
....TH
......
......
s.....
"
    (is (= (multi-step
            {:H [1 3]
             :T [2 4]}
            [:R 4])
           {:H [5 3]
            :T [4 3]})))
  
  (testing "== D 1 ==
......
....TH
......
......
s.....
            
......
....T.
.....H
......
s.....
"
    (is (= (multi-step
            {:H [5 3]
             :T [4 3]}
            [:D 1])
           {:H [5 2]
            :T [4 3]})))
  (testing "== L 5 ==
......
....T.
.....H
......
s.....
            
......
......
HT....
......
s.....
"
    (is (= (multi-step
            {:H [5 2]
             :T [4 3]}
            [:L 5])
           {:H [0 2]
            :T [1 2]})))
  (testing "== R 2 ==
......
......
HT....
......
s.....            

......
......
.TH...
......
s....."
    (is (= (multi-step
            {:H [0 2]
             :T [1 2]}
            [:R 2])
           {:H [2 2]
            :T [1 2]}))))

(deftest solve-example1-test
  (testing "solve example"
    (is (= (-> "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
               tail-visited
               second
               count)
           13)))
  (testing "testing visited
..##..
...##.
.####.
....#.
s###..
"(comment 
   #{[4 3] [2 2] [1 0] [2 3] [3 3] [3 4] [4 2] [3 0] [4 1] [2 4] [2 0] [1 2] [3 2]})
    (is 
     (= (-> "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
            tail-visited
            second)
       #{[4 3] [2 2] [1 0] [2 3] [3 3] [3 4] [4 2] [3 0] [4 1] [2 4] [2 0] [1 2] [3 2]} ))))

(deftest solve-part1
  (-> (slurp "./day9_input.txt")
      tail-visited
      second
      count))



  
  
    
  