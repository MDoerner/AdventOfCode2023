(ns advent-of-code-2023.days.day7-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day7 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 7)
(def example_input_1 "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input [
                          {:cards ["3" "2" "T" "3" "K"] :bid 765}
                          {:cards ["T" "5" "5" "J" "5"] :bid 684}
                          {:cards ["K" "K" "6" "7" "7"] :bid 28}
                          {:cards ["K" "T" "J" "J" "T"] :bid 220}
                          {:cards ["Q" "Q" "Q" "J" "A"] :bid 483}
                          ]]
      (is (= parsed_input expected_input)))))


(deftest solve_part1-example-test
  (testing "Test for example input to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "6440")))))

(deftest solve_part2-example-test
  (testing "Test for example input to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "5905")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "250951660"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "251481660"))))

