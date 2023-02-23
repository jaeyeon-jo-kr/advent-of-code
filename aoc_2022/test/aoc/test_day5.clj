(ns aoc.test-day5
  (:require [aoc.day5 :refer :all]
            [clojure.test :refer :all]))

(deftest crate-creates
  (testing "->crates"
    (is (= (->crates "[D]") :D))
    (is (= (->crates "   ") nil))))

(deftest stack-of-crate
  (testing "stack of crates"
    (is (= (stack-of-create "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 ")
           '((:N :Z) (:D :C :M) (:P))))))

(deftest test-parse-event
  (testing "parse event"
    (is (= (->command "move 1 from 2 to 1")
           {:move 1 :from 2 :to 1}))))
(def sample "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(deftest test-parse
  (testing "parse-input
                [D]    
            [N] [C]    
            [Z] [M] [P]
             1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"
    (is (= (parse sample)
           ['((:N :Z) (:D :C :M) (:P))
            [{:move 1 :from 2 :to 1}
             {:move 3 :from 1 :to 3}
             {:move 2 :from 2 :to 1}
             {:move 1 :from 1 :to 2}]]))))

(deftest test-move-event
  (testing "
                [D]    
            [N] [C]    
            [Z] [M] [P]
            1   2   3 
            Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:

           [D]        
           [N] [C]    
           [Z] [M] [P]
            1   2   3 
"
    (is (= (move '((:N :Z) (:D :C :M) (:P))
                 {:move 1 :from 2 :to 1})
           '((:D :N :Z) (:C :M) (:P)))))
  (testing "[D]        
            [N] [C]    
            [Z] [M] [P]
             1   2   3
            In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:
                    [Z]
                    [N]
                [C] [D]
                [M] [P]
             1   2   3"
    (is (= (move '((:D :N :Z) (:C :M) (:P))
                 {:move 3 :from 1 :to 3})
           '(() (:C :M) (:Z :N :D :P)))))
  (testing "        [Z]
                    [N]
                [C] [D]
                [M] [P]
             1   2   3
Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:
                    [Z]
                    [N]
            [M]     [D]
            [C]     [P]
             1   2   3"
    (is (= (move '(() (:C :M) (:Z :N :D :P))
                 {:move 2 :from 2 :to 1})
           '((:M :C) () (:Z :N :D :P)))))
  (testing "        [Z]
                    [N]
            [M]     [D]
            [C]     [P]
             1   2   3
            Finally, one crate is moved from stack 1 to stack 2:
                    [Z]
                    [N]
                    [D]
            [C] [M] [P]
             1   2   3"
    (is (= (move '((:M :C) () (:Z :N :D :P))
                 {:move 1 :from 1 :to 2})
           '((:C) (:M) (:Z :N :D :P))))))

(deftest solve-sample
  (testing "solving sample" 
    (is (= (top-of-stacks ['((:N :Z) (:D :C :M) (:P))
                           [{:move 1 :from 2 :to 1}
                            {:move 3 :from 1 :to 3}
                            {:move 2 :from 2 :to 1}
                            {:move 1 :from 1 :to 2}]])
           "CMZ"))))

(deftest solve-part1
  (testing "solving part 1"
    (-> (slurp "day5_input.txt")
        parse
        top-of-stacks)))

(deftest test-move2-event
  (testing "
                [D]    
            [N] [C]    
            [Z] [M] [P]
            1   2   3 
          
Moving a single crate from stack 2 to stack 1 behaves the same as before:
           [D]        
           [N] [C]    
           [Z] [M] [P]
            1   2   3
"
    (is (= (move-2 '((:N :Z) (:D :C :M) (:P))
                 {:move 1 :from 2 :to 1})
           '((:D :N :Z) (:C :M) (:P)))))
  (testing "[D]        
[N] [C]    
[Z] [M] [P]
 1   2   3
            
However, the action of moving three crates from stack 1 to stack 3 means that those three moved crates stay in the same order, resulting in this new configuration:
        [D]
        [N]
    [C] [Z]
    [M] [P]
 1   2   3"
    (is (= (move-2 '((:D :N :Z) (:C :M) (:P))
                 {:move 3 :from 1 :to 3})
           '(() (:C :M) (:D :N :Z :P)))))
  (testing "        [D]
        [N]
    [C] [Z]
    [M] [P]
 1   2   3
Next, as both crates are moved from stack 2 to stack 1, they retain their order as well:
        [D]
        [N]
[C]     [Z]
[M]     [P]
 1   2   3"
    (is (= (move-2 '(() (:C :M) (:D :N :Z :P))
                 {:move 2 :from 2 :to 1})
           '((:C :M) () (:D :N :Z :P)))))
  (testing 
   "        [D]
        [N]
[C]     [Z]
[M]     [P]
 1   2   3
            Finally, a single crate is still moved from stack 1 to stack 2, but now it's crate C that gets moved:
                    [D]
        [N]
        [Z]
[M] [C] [P]"
    (is (= (move '((:C :M) () (:D :N :Z :P))
                 {:move 1 :from 1 :to 2})
           '((:M) (:C) (:D :N :Z :P))))))

(deftest solve-part2
  (testing "solving part 2"
    (-> (slurp "day5_input.txt")
        parse
        top-of-stacks-2)))
