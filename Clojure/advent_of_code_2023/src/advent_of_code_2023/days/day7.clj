(ns advent-of-code-2023.days.day7
  (:require [clojure.string :as str])
  (:require [clojure.math :as math]))


(def hand_regex #"(\w+) (\d+)")

(defn- parse_hand
  [hand_text]
  (let [hand_match (re-find hand_regex hand_text)
        cards_text (get hand_match 1)
        bid_text (get hand_match 2)]
    {
     :cards (into [] (str/split cards_text #""))
     :bid (Integer/parseInt bid_text)
     }))


(defn parse_input
  "Parses the input for day 7 of AdventOfCode 2023"
  [input_string]
  (let [lines (remove str/blank? (str/split input_string #"\r?\n"))
        hands (map parse_hand lines)]
    (into [] hands)))


(defn- hand_type
  [{cards :cards}]
  (let [card_counts (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} cards)]
    (case (-> card_counts (keys) (count))
      5 "High card"
      4 "One pair"
      3 (if (some #(= %1 3) (vals card_counts))
          "Three of a kind"
          "Two pair")
      2 (if (some #(= %1 3) (vals card_counts))
          "Full house"
          "Four of a kind")
      1 "Five of a kind")))

(defn- card_score
  [card]
  (case card
    "A" 12
    "K" 11
    "Q" 10
    "J" 9
    "T" 8
    (- (Integer/parseInt card) 2)))

(defn- type_score
  [type_of_hand]
  (case type_of_hand
    "Five of a kind" 6
    "Four of a kind" 5
    "Full house" 4
    "Three of a kind" 3
    "Two pair" 2
    "One pair" 1
    "High card" 0))

(defn- hand_score
  [{cards :cards type_of_hand :type_of_hand}]
  (let [type_contribution (* (type_score type_of_hand) (int (math/pow 13 5)))
        cards_contribution (->> cards
                                (reverse)
                                (map-indexed #(* (int (math/pow 13 %1)) (card_score %2)))
                                (reduce +))]
    (+ type_contribution cards_contribution)))

(defn- scored_hand
  [hand]
  (let [with_type (assoc hand :type_of_hand (hand_type hand))]
    (assoc with_type :score (hand_score with_type))))

  (defn- aggregate_score
    [hands]
    (->> hands
         (map scored_hand)
         (sort-by :score)
         (map-indexed #(* (inc %1) (:bid %2)))
         (reduce +)
         ))

(defn solve_part1
  "Solves part 1 of day 7 of AdventOfCode 2023"
  [input]
  (str (aggregate_score input)))



; Part 2 could be integrated by injecting the card_score and hand_type functions.
; However, for the coding challenge, the effort seems wasted.

(defn- joker_card_score
  [card]
  (case card
    "A" 12
    "K" 11
    "Q" 10
    "J" 0
    "T" 9
    (dec (Integer/parseInt card))))

(defn- joker_hand_type
  [{cards :cards}]
  (->> ["A" "K" "Q" "T" "9" "8" "7" "6" "5" "4" "3" "2"]
       (map #(replace {"J" %1} cards))
       (map (fn [cs] {:cards cs}))
       (map scored_hand)
       (sort-by :score)
       (reverse)
       (first)
       (:type_of_hand)))

(defn- joker_hand_score
  [{cards :cards type_of_hand :type_of_hand}]
  (let [type_contribution (* (type_score type_of_hand) (int (math/pow 13 5)))
        cards_contribution (->> cards
                                (reverse)
                                (map-indexed #(* (int (math/pow 13 %1)) (joker_card_score %2)))
                                (reduce +))]
    (+ type_contribution cards_contribution)))

(defn- joker_scored_hand
  [hand]
  (let [with_type (assoc hand :type_of_hand (joker_hand_type hand))]
    (assoc with_type :score (joker_hand_score with_type))))

(defn- aggregate_joker_score
  [hands]
  (->> hands
       (map joker_scored_hand)
       (sort-by :score)
       (map-indexed #(* (inc %1) (:bid %2)))
       (reduce +)
       ))

(defn solve_part2
  "Solves part 2 of day 7 of AdventOfCode 2023"
  [input]
  (str (aggregate_joker_score input)))
