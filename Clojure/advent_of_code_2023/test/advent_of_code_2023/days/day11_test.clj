(ns advent-of-code-2023.days.day11-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day11 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 11)
(def example_input_1 "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          expected_input #{
                           {:x 3 :y 0}
                           {:x 7 :y 1}
                           {:x 0 :y 2}
                           {:x 6 :y 4}
                           {:x 1 :y 5}
                           {:x 9 :y 6}
                           {:x 7 :y 8}
                           {:x 0 :y 9}
                           {:x 4 :y 9}
                          }]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "374")))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "1")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "9623138"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "726820169514"))))

