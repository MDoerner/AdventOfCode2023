(ns advent-of-code-2023.days.day15
  (:require [clojure.string :as str]))


(defn parse_input
  "Parses the input for day 15 of AdventOfCode 2023"
  [input_string]
  (into [] (-> input_string
               (str/split #"\r?\n")
               (first)
               (str/split #","))))

(defn- holiday_hash
  [text]
  (->> text
       (map int)
       (reduce #(mod (* 17 (+ %1 %2)) 256) 0)))

(defn solve_part1
  "Solves part 1 of day 15 of AdventOfCode 2023"
  [input]
  (->> input
       (map holiday_hash)
       (reduce +)
       (str)))

(defn- parse_command
  [text]
  (let [parse_result (re-find #"^(.+)([=-])(\d?)$" text)
        label (get parse_result 1)
        operator (get parse_result 2)]
    (if (= operator "=")
      {:label label :operator operator :focal (Integer/parseInt (get parse_result 3))}
      {:label label :operator operator})))

(defn- remove_lens
  [hashmap label]
  (let [box (holiday_hash label)]
    (if (contains? hashmap box)
      (let [old_lenses (get hashmap box)
            new_lenses (->> old_lenses
                         (filter #(not= label (:label %1)))
                         (into []))]
      (if (= (count old_lenses) (count new_lenses))
        hashmap
        (if (empty? new_lenses)
          (dissoc hashmap box)
          (assoc hashmap box new_lenses))))
      hashmap)))

(defn- first_index_where
  [v pred]
  (let [length (count v)]
    (loop [index 0]
      (if (>= index length)
        -1
        (let [item (get v index)]
          (if (pred item)
            index
            (recur (inc index))))))))

(defn- insert_lens
  [hashmap label focal]
  (let [box (holiday_hash label)]
    (if (contains? hashmap box)
      (let [old_lenses (get hashmap box)
            label_index (first_index_where old_lenses #(= label (:label %1)))
            new_lenses (if (= -1 label_index)                              ;not contained
                         (conj old_lenses {:label label :focal focal})
                         (update old_lenses label_index #(assoc %1 :focal focal)))]
        (assoc hashmap box new_lenses))
      (assoc hashmap box [{:label label :focal focal}]))))

(defn- apply_command
  [hashmap command]
  (case (:operator command)
    "-" (remove_lens hashmap (:label command))
    "=" (insert_lens hashmap (:label command) (:focal command))))


(defn- initialise
  [commands]
  (reduce apply_command {} commands))

(defn- focusing_power
  [hashmap]
  (->> hashmap
    (map (fn [[n items]]
           (*
             (inc n)
             (->> items
               (map-indexed (fn [index {focal :focal}] (* (inc index) focal)))
               (reduce +)))))
    (reduce +)))

(defn solve_part2
  "Solves part 2 of day 15 of AdventOfCode 2023"
  [input]
  (->> input
       (map parse_command)
       (initialise)
       (focusing_power)
       (str)))
