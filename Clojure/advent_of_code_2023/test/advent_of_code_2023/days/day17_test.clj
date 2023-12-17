(ns advent-of-code-2023.days.day17-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day17 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 17)
(def example_input_1 "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533\n")
(def example_input_2 "111111111111\n999999999991\n999999999991\n999999999991\n999999999991\n")

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          expected_input  [
                           [2 4 1 3 4 3 2 3 1 1 3 2 3]
                           [3 2 1 5 4 5 3 5 3 5 6 2 3]
                           [3 2 5 5 2 4 5 6 5 4 2 5 4]
                           [3 4 4 6 5 8 5 8 4 5 4 5 2]
                           [4 5 4 6 6 5 7 8 6 7 5 3 6]
                           [1 4 3 8 5 9 8 7 9 8 4 5 4]
                           [4 4 5 7 8 7 6 9 8 7 7 6 6]
                           [3 6 3 7 8 7 7 9 7 9 6 5 3]
                           [4 6 5 4 9 6 7 9 8 6 8 8 7]
                           [4 5 6 4 6 7 9 9 8 6 4 5 3]
                           [1 2 2 4 6 8 6 8 6 5 5 6 3]
                           [2 5 4 6 5 4 8 8 8 7 7 3 5]
                           [4 3 2 2 6 7 4 6 5 5 5 3 3]
                           ]]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "102")))))

(deftest solve_part2-example1-test
  (testing "Test for example input 1 to part 2"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "94")))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "71")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "953"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "1180"))))

