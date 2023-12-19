(ns advent-of-code-2023.days.day18-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day18 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 18)
(def example_input_1 "R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          expected_input  [
                           {:direction :right :length 6 :color "#70c710"}
                           {:direction :down :length 5 :color "#0dc571"}
                           {:direction :left :length 2 :color "#5713f0"}
                           {:direction :down :length 2 :color "#d2c081"}
                           {:direction :right :length 2 :color "#59c680"}
                           {:direction :down :length 2 :color "#411b91"}
                           {:direction :left :length 5 :color "#8ceee2"}
                           {:direction :up :length 2 :color "#caa173"}
                           {:direction :left :length 1 :color "#1b58a2"}
                           {:direction :up :length 2 :color "#caa171"}
                           {:direction :right :length 2 :color "#7807d2"}
                           {:direction :up :length 3 :color "#a77fa3"}
                           {:direction :left :length 2 :color "#015232"}
                           {:direction :up :length 2 :color "#7a21e3"}
                           ]]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "62")))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "952408144115")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "67891"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "94116351948493"))))

