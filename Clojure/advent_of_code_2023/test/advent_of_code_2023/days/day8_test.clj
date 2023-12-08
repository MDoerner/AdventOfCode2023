(ns advent-of-code-2023.days.day8-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day8 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 8)
(def example_input_1 "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)\n")
(def example_input_2 "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)\n")
(def example_input_3 "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)\n")

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input {
                          :directions [:right :left]
                          :desert_map {
                                "AAA" {:left "BBB" :right "CCC"}
                                "BBB" {:left "DDD" :right "EEE"}
                                "CCC" {:left "ZZZ" :right "GGG"}
                                "DDD" {:left "DDD" :right "DDD"}
                                "EEE" {:left "EEE" :right "EEE"}
                                "GGG" {:left "GGG" :right "GGG"}
                                "ZZZ" {:left "ZZZ" :right "ZZZ"}
                                }
                          }]
      (is (= parsed_input expected_input)))))


(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "2")))))

(deftest solve_part1-example2-test
  (testing "Test for example input 2 to part 1"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "6")))))

(deftest solve_part2-example-test
  (testing "Test for example input to part 2"
    (let [input_text example_input_3
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "6")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "17287"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "18625484023687"))))

