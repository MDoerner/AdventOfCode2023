(ns advent-of-code-2023.days.day21-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day21 :refer [parse_input solve_part1 solve_part2 solve_part2_naive]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 21)
(def example_input_1 "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n...........\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input {
                          :steps1 64
                          :setps2 26501365
                          :start {:x 5 :y 5}
                          :width 11
                          :height 11
                          :rocks #{
                                   {:x 5 :y 1}
                                   {:x 6 :y 1}
                                   {:x 7 :y 1}
                                   {:x 9 :y 1}
                                   {:x 1 :y 2}
                                   {:x 2 :y 2}
                                   {:x 3 :y 2}
                                   {:x 5 :y 2}
                                   {:x 6 :y 2}
                                   {:x 9 :y 2}
                                   {:x 2 :y 3}
                                   {:x 4 :y 3}
                                   {:x 8 :y 3}
                                   {:x 4 :y 4}
                                   {:x 6 :y 4}
                                   {:x 1 :y 5}
                                   {:x 2 :y 5}
                                   {:x 6 :y 5}
                                   {:x 7 :y 5}
                                   {:x 8 :y 5}
                                   {:x 9 :y 5}
                                   {:x 1 :y 6}
                                   {:x 2 :y 6}
                                   {:x 5 :y 6}
                                   {:x 9 :y 6}
                                   {:x 7 :y 7}
                                   {:x 8 :y 7}
                                   {:x 1 :y 8}
                                   {:x 2 :y 8}
                                   {:x 4 :y 8}
                                   {:x 6 :y 8}
                                   {:x 7 :y 8}
                                   {:x 8 :y 8}
                                   {:x 9 :y 8}
                                   {:x 1 :y 9}
                                   {:x 2 :y 9}
                                   {:x 5 :y 9}
                                   {:x 6 :y 9}
                                   {:x 8 :y 9}
                                   {:x 9 :y 9}
                                  }
                          }]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          adjusted_input (assoc parsed_input :steps1 6)
          result (solve_part1 adjusted_input)]
      (is (= result "16")))))

(deftest solve_part2_naive-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          adjusted_inputs (into [] (map #(assoc parsed_input :steps2 %1) [6 10 50 100 500 1000 5000]))
          results (into [] (map #(time (solve_part2_naive %1)) adjusted_inputs))
          expecteds ["16" "50" "1594" "6536" "167004" "668697" "16733044"]]
      (is (= results expecteds)))))



(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "3724"))))


(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "620348631910321"))))

