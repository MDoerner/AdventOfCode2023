(ns advent-of-code-2023.days.day2
  (:require [clojure.string :as str]))


(def game_regex #"^Game (\d+): (.*)$")
(def gem_regex #"(\d+) ((blue)|(green)|(red))")




(defn- parse_shown_gem_type
  "Parses a revealed number of gems of one type"
  [gem_text]
  (let [gem_match (re-find gem_regex gem_text)
        gem_count (Integer/parseInt (get gem_match 1))
        gem_type (case (get gem_match 2)
                   "red" :red
                   "blue" :blue
                   "green" :green)]
    {gem_type gem_count}))

(defn- lager_gem_count
  "Returns a gem set with the larger count of gems per type"
  [gem_set other_gem_set]
  (let [{red :red blue :blue green :green, :or {red 0 blue 0 green 0}} gem_set
        {other_red :red other_blue :blue other_green :green, :or {other_red 0 other_blue 0 other_green 0}} other_gem_set]
    {
     :red (if (> other_red red) other_red red)
     :blue (if (> other_blue blue) other_blue blue)
     :green (if (> other_green green) other_green green)
     }))


(defn- parse_reveal
  "Parses the reveals of a single game"
  [reveals_text]
  (let [shown_gem_texts (str/split reveals_text #",")
        parsed_gem_sets (map parse_shown_gem_type shown_gem_texts)]
    (reduce lager_gem_count {:red 0 :blue 0 :green 0} parsed_gem_sets)))

(defn- parse_reveals
  "Parses the reveals of a single game"
  [reveals_text]
  (map
    parse_reveal
    (str/split reveals_text #";")))

(defn- parse_game
  "Parses the input for a single game"
  [game_text]
  (let [game_match (re-find game_regex game_text)
        game_id (Integer/parseInt (get game_match 1))
        reveals (parse_reveals (get game_match 2))]
    {:game game_id :reveals reveals}))

(defn parse_input
  "Parses the input for day 2 of AdventOfCode 2023"
  [input_string]
  (map
    parse_game
    (remove
      str/blank?
      (str/split input_string #"\r?\n"))))


(defn- required_gems
  [reveals]
  (reduce lager_gem_count {:red 0 :blue 0 :green 0} reveals))

(defn- with_required_gems
  [game]
  (assoc game :required_gems (required_gems (:reveals game))))

(defn- any_gem_count_larger
  [gem_set reference]
  (or (> (:red gem_set) (:red reference))
         (> (:blue gem_set) (:blue reference))
         (> (:green gem_set) (:green reference))))

(defn- admissible_game_id_sum
  [games, reference]
  (let [games_with_required_gems (map with_required_gems games)
        admissible_games (remove #(any_gem_count_larger (:required_gems %1) reference) games_with_required_gems)
        game_ids (map :game admissible_games)]
    (reduce + game_ids)
    ))

(defn solve_part1
  "Solves part 1 of day 2 of AdventOfCode 2023"
  [input]
  (str (admissible_game_id_sum input {:red 12 :green 13 :blue 14})))


(defn- gem_set_power
  [gem_set]
  (let [{red :red green :green blue :blue, :or {red 0 green 0 blue 0}} gem_set]
    (* red blue green)))

(defn- total_games_power
  [games]
  (let [reveals_per_game (map :reveals games)
        required_gem_per_game (map required_gems reveals_per_game)
        game_powers (map gem_set_power required_gem_per_game)]
    (reduce + game_powers)
    ))

(defn solve_part2
  "Solves part 2 of day 2 of AdventOfCode 2023"
  [input]
  (str (total_games_power input)))
