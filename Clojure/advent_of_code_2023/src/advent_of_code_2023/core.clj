(ns advent-of-code-2023.core
  (:gen-class)
  (:require [advent-of-code-2023.input-reader :as in]
            [advent-of-code-2023.days.core :as days]))

(defn solve_problem
  "Solve the specified part of the specified day of AdventOfCode 2023"
  [day, part]
  (let [input_string (in/load_problem_input day)
        input (days/parse_day day input_string)]
     (time (days/solve_part day part input))))


(defn -main [& args]
  (let [day (get args 0)
        part (get args 1)]
    (println (solve_problem day part))))