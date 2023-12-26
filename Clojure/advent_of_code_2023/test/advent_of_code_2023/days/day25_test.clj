(ns advent-of-code-2023.days.day25-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day25 :refer [parse_input solve_part1]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 25)
(def example_input_1 "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr\n")

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input  {
                           "jqt" #{"rhn" "xhk" "nvd"}
                           "rsh" #{"frs" "pzl" "lsr"}
                           "xhk" #{"hfx"}
                           "cmg" #{"qnr" "nvd" "lhk" "bvb"}
                           "rhn" #{"xhk" "bvb" "hfx"}
                           "bvb" #{"xhk" "hfx"}
                           "pzl" #{"lsr" "hfx" "nvd"}
                           "qnr" #{"nvd"}
                           "ntq" #{"jqt" "hfx" "bvb" "xhk"}
                           "nvd" #{"lhk"}
                           "lsr" #{"lhk"}
                           "rzs" #{"qnr" "cmg" "lsr" "rsh"}
                           "frs" #{"qnr" "lhk" "lsr"}
                           }]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (time (solve_part1 parsed_input))]
      (is (= result "54")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "612945"))))
