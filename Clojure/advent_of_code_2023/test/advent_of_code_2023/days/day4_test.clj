(ns advent-of-code-2023.days.day4-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day4 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 4)
(def example_input_1 "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input [
                          {:card 1 :winning #{41 48 83 86 17} :numbers [83 86  6 31 17  9 48 53]}
                          {:card 2 :winning #{13 32 20 16 61} :numbers [61 30 68 82 17 32 24 19]}
                          {:card 3 :winning #{ 1 21 53 59 44} :numbers [69 82 63 72 16 21 14  1]}
                          {:card 4 :winning #{41 92 73 84 69} :numbers [59 84 76 51 58  5 54 83]}
                          {:card 5 :winning #{87 83 26 28 32} :numbers [88 30 70 12 93 22 82 36]}
                          {:card 6 :winning #{31 18 13 56 72} :numbers [74 77 10 23 35 67 36 11]}
                          ]]
      (is (= parsed_input expected_input)))))


(deftest solve_part1-example-test
  (testing "Test for example input to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "13")))))

(deftest solve_part2-example-test
  (testing "Test for example input to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "30")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "18619"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "8063216"))))

