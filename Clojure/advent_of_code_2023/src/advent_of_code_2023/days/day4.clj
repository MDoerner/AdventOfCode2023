(ns advent-of-code-2023.days.day4
  (:require [clojure.string :as str])
  (:require [clojure.math :as math]))

(def card_regex #"^Card\s+(\d+): (.+) \| (.+)$")

 (defn- parse_card
   [card_text]
   (let [card_match (re-find card_regex card_text)
         card_id (Integer/parseInt (get card_match 1))
         winning_numbers (into #{} (map #(Integer/parseInt %1) (re-seq #"\d+" (get card_match 2))))
         numbers (into [] (map #(Integer/parseInt %1) (re-seq #"\d+" (get card_match 3))))]
     {:card card_id :winning winning_numbers :numbers numbers}))

(defn parse_input
  "Parses the input for day 4 of AdventOfCode 2023"
  [input_string]
  (map
    parse_card
    (remove
      str/blank?
      (str/split input_string #"\r?\n"))))

(defn- card_score
  [card]
  (let [{winning :winning numbers :numbers} card
        matching_numbers (filter #(contains? winning %1) numbers)
        matching_count (count matching_numbers)]
    (if (= matching_count 0)
      0
      (int (math/pow 2 (dec matching_count))))))

(defn- total_score
  [cards]
  (reduce + (map card_score cards)))

(defn solve_part1
  "Solves part 1 of day 4 of AdventOfCode 2023"
  [input]
  (str (total_score input)))

(defn- winning_count
  [card]
  (let [{winning :winning numbers :numbers} card
        matching_numbers (filter #(contains? winning %1) numbers)]
    (count matching_numbers)))

(defn- winning_counts
  [cards]
  (into (sorted-map) (map (fn [card] [(:card card) (winning_count card)]) cards)))

(defn- add_won_cards
  [card_counts [id won_cards_count]]
  (let [cards_owned (get card_counts id)
        won_cards (map #(+ id %1) (range 1 (inc won_cards_count)))]
    (reduce #(update %1 %2 (fn [old_count] (+ cards_owned old_count) )) card_counts won_cards)))

(defn- card_counts
  [cards]
  (let [winning (winning_counts cards)
        initial_card_counts (update-vals winning (fn [_] 1))]
    (reduce add_won_cards initial_card_counts winning)))


(defn solve_part2
  "Solves part 2 of day 4 of AdventOfCode 2023"
  [input]
  (str (reduce + (vals (card_counts input)))))
