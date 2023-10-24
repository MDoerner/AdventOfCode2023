(ns advent-of-code-2023.input-reader
  (:require [clojure.java.io :as io]))


(defn problem_url
  "Returns the URL of the resource containing the problem input for the specified day of AdventOfCode 2023"
  [day]
  (let [file_name (str "Day" day ".txt")]
    (io/resource file_name)))


(defn load_problem_input
  "Loads the problem input for the specified day of AdventOfCode 2023"
  [day]
  (slurp (problem_url day)))