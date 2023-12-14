(ns advent-of-code-2023.days.day14-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day14 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 14)
(def example_input_1 "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#....\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input {
                          :width 10
                          :height 10
                          :round #{
                                   {:x 0 :y 0}
                                   {:x 0 :y 1}
                                   {:x 2 :y 1}
                                   {:x 3 :y 1}
                                   {:x 0 :y 3}
                                   {:x 1 :y 3}
                                   {:x 4 :y 3}
                                   {:x 9 :y 3}
                                   {:x 1 :y 4}
                                   {:x 7 :y 4}
                                   {:x 0 :y 5}
                                   {:x 5 :y 5}
                                   {:x 2 :y 6}
                                   {:x 6 :y 6}
                                   {:x 9 :y 6}
                                   {:x 7 :y 7}
                                   {:x 1 :y 9}
                                   {:x 2 :y 9}
                                   }
                          :square #{
                                    {:x 5 :y 0}
                                    {:x 4 :y 1}
                                    {:x 9 :y 1}
                                    {:x 5 :y 2}
                                    {:x 6 :y 2}
                                    {:x 3 :y 3}
                                    {:x 8 :y 4}
                                    {:x 2 :y 5}
                                    {:x 7 :y 5}
                                    {:x 9 :y 5}
                                    {:x 5 :y 6}
                                    {:x 0 :y 8}
                                    {:x 5 :y 8}
                                    {:x 6 :y 8}
                                    {:x 7 :y 8}
                                    {:x 0 :y 9}
                                    {:x 5 :y 9}
                                    }
                           }]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "136")))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "64")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "112048"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "105606"))))

