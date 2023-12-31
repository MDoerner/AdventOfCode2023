(ns advent-of-code-2023.days.core)

(defn- day_namespace_symbol
  "Returns the symbol of the namespace containing the solution to the specified day of AdventOfCode 2023"
  [day]
  (symbol (str "advent-of-code-2023.days.day" day)))

(defn parse_day
  "Parse input of AdventOfCode 2023 problem for specified day"
  [day input_string]
  (let [namespace_symbol (day_namespace_symbol day)]
    (require namespace_symbol)
    (let [function_symbol (symbol "parse_input")
          parser (ns-resolve namespace_symbol function_symbol)]
      (apply parser [input_string]))))


(defn solve_part
  "Solves specified part of AdventOfCode 2023 problem for specified day based on parsed input"
  [day part input]
  (let [namespace_symbol (day_namespace_symbol day)]
    (require namespace_symbol)
    (let [function_symbol (symbol (str "solve_part" part))
          solver (ns-resolve namespace_symbol function_symbol)]
      (apply solver [input]))))