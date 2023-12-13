(ns advent-of-code-2023.days.day13-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day13 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 13)
(def example_input_1 "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input [
                          {
                           :horizontal [205 180 259 295 180 204 181]
                           :vertical [77 12 115 33 82 82 33 115 12]
                           }
                          {
                           :horizontal [305 289 460 223 223 460 289]
                           :vertical [91 24 60 60 25 67 60 60 103]
                           }
                          ]]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "405")))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "400")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "34202"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "34230"))))

