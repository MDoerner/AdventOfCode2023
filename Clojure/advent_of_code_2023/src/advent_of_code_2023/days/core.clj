(ns advent-of-code-2023.days.core)

(defn- day_namespace_symbol
  "Returns the symbol of the namespace containing the solution to the specified day of AdventOfCode 2023"
  [day]
  (symbol (str "advent-of-code-2023.days.day" day)))

(defn parse_day
  "Parse input of AdventOfCode 2023 problem for specified day"
  [day, input_string]
  (let [namespace_symbol (day_namespace_symbol day)
        function_name "parse_input"
        parser (symbol namespace_symbol function_name)]
    (require namespace_symbol)
    (parser input_string)))

(defn solve_part
  "Solves specified part of AdventOfCode 2023 problem for specified day based on parsed input"
  [day, part, input]
  (let [namespace_symbol (day_namespace_symbol day)
        function_name (str "solve_part" part)
        solver (symbol namespace_symbol function_name)]
    (require namespace_symbol)
    (solver input)))