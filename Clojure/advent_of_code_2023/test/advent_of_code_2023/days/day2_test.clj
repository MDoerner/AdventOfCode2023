(ns advent-of-code-2023.days.day2-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day2 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 2)
(def example_input_1 "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input '(
                           {:game 1 :reveals (
                                             {:red 4 :blue 3 :green 0}
                                             {:red 1 :blue 6 :green 2}
                                             {:red 0 :blue 0 :green 2}
                                             )}
                          {:game 2 :reveals (
                                             {:red 0 :blue 1 :green 2}
                                             {:red 1 :blue 4 :green 3}
                                             {:red 0 :blue 1 :green 1}
                                             )}
                          {:game 3 :reveals (
                                             {:red 20 :blue 6 :green 8}
                                             {:red 4 :blue 5 :green 13}
                                             {:red 1 :blue 0 :green 5}
                                             )}
                          {:game 4 :reveals (
                                             {:red 3 :blue 6 :green 1}
                                             {:red 6 :blue 0 :green 3}
                                             {:red 14 :blue 15 :green 3}
                                             )}
                          {:game 5 :reveals (
                                             {:red 6 :blue 1 :green 3}
                                             {:red 1 :blue 2 :green 2}
                                             )}

                           )]
      (is (= parsed_input expected_input)))))


(deftest solve_part1-example-test
  (testing "Test for example input to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "8")))))

(deftest solve_part2-example-test
  (testing "Test for example input to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "2286")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "2164"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "69929"))))

