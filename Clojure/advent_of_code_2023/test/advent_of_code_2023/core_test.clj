(ns advent-of-code-2023.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2023.core :refer :all]))


(deftest solve_problem-test
  (is (= (solve_problem 1 1) "55123")))
