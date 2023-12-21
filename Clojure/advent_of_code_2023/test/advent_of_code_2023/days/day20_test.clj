(ns advent-of-code-2023.days.day20-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day20 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 20)
(def example_input_1 "broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a\n")
(def example_input_2 "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output\n%output -> rx\n")
(def example_input_3 "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output\n")

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_3
          parsed_input (parse_input input_text)
          expected_input {
                          :modules {
                                    "button" {:id "button" :type :button :targets ["broadcaster"]}
                                    "broadcaster" {:id "broadcaster" :type :broadcast :targets ["a"]}
                                    "a" {:id "a" :type :flip-flop :targets ["inv" "con"]}
                                    "inv" {:id "inv" :type :conjunction :targets ["b"]}
                                    "b" {:id "b" :type :flip-flop :targets ["con"]}
                                    "con" {:id "con" :type :conjunction :targets ["output"]}
                                    }
                          :state {
                                  "a" :off
                                  "b" :off
                                  "inv" {"a" :low}
                                  "con" {"a" :low "b" :low}
                                  }}]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "32000000")))))

(deftest solve_part1-example3-test
  (testing "Test for example input 3 to part 1"
    (let [input_text example_input_3
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "11687500")))))

;(deftest solve_part2-example2-test
;  (testing "Test for example input 2 to part 2"
;    (let [input_text example_input_2
;          parsed_input (parse_input input_text)
;          result (solve_part2 parsed_input)]
;      (is (= result "3")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "777666211"))))


(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "243081086866483"))))

