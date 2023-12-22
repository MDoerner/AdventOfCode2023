(ns advent-of-code-2023.days.day22
  (:require [clojure.string :as str]))

(defn- parse_point
  [text]
  (let [coords (->> (str/split text #",")
                    (map #(Integer/parseInt %1))
                    (into []))]
    {:x (get coords 0) :y (get coords 1) :z (get coords 2)}))

(defn- parse_block
  [text]
  (let [parts (str/split text #"~")]
    (into [] (map parse_point parts))))

(defn parse_input
  "Parses the input for day 22 of AdventOfCode 2023"
  [input_string]
  (->> (str/split input_string #"\r?\n")
       (remove str/blank?)
       (map parse_block)
       (into []))
  )


(defn- ascending_point_key
  [{x :x y :y z :z}]
  [z y x])

(defn- ascending_block_key
  [block]
  (let [points (sort-by ascending_point_key block)]
    (->> points
         (map ascending_point_key)
         (into []))))

(defn- drop_block
  [{top :top supports :supports supported_by :supported_by} {id :id block :block}]
  (let [[p1 p2] (into []  (sort-by ascending_point_key block))
        ground_projection (for [x (range (:x p1) (inc (:x p2)))
                               y (range (:y p1) (inc (:y p2)))]
                           {:x x :y y})
        below (->> ground_projection
                   (map #(get top %1))
                   (filter some?))
        highest_below (if (empty? below) nil (apply max-key :z below))
        new_top_z (+ (inc (- (:z p2) (:z p1))) (if (nil? highest_below) 0 (:z highest_below)))
        new_top (reduce #(assoc %1 %2 {:id id :z new_top_z}) top ground_projection)]
    (if (nil? highest_below)
      {:top new_top :supports supports :supported_by supported_by})
      (let [block_supported_by (->> below
                                (filter #(= (:z %1) (:z highest_below)))
                                (map :id)
                                (into #{}))
            new_supported_by (assoc supported_by id block_supported_by)
            new_supports (reduce
                           #(update %1 %2 (fn [old_supports]
                                            (if (nil? old_supports)
                                              #{id}
                                              (conj old_supports id))))
                           supports
                           block_supported_by)]
        {:top new_top :supports new_supports :supported_by new_supported_by})))

(defn- drop_blocks
  [ascending_blocks]
  (reduce drop_block {:top {} :supports {} :supported_by {}} ascending_blocks))

(defn- can-be-removed?
  [id supported_by supports]
  (->> (get supports id [])
       (every? #(> (count (get supported_by %1)) 1))))

(defn- removable_blocks
  [ascending_blocks]
  (let [{supports :supports supported_by :supported_by} (drop_blocks ascending_blocks)
        ids (into #{} (map :id ascending_blocks))]
    (->> ids
         (filter #(can-be-removed? %1 supported_by supports))
         (into #{}))))

(defn- indexed_blocks
  "Blocks indexed from bottom to top in snapshot"
  [blocks]
  (let [ascending (sort-by ascending_block_key blocks)
        indexed_blocks (map-indexed (fn [index block] {:id index :block block}) ascending)]
    (into [] indexed_blocks)))

(defn solve_part1
  "Solves part 1 of day 22 of AdventOfCode 2023"
  [input]
  (let [ascending_blocks (indexed_blocks input)
        removable (removable_blocks ascending_blocks)]
    (str (count removable))))

(defn- looses-support?
  [id supported_by falling]
  (->> (get supported_by id [])
       (every? #(contains? falling %1))))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn- falling_blocks
  [id supported_by supports]
  (loop [falling #{id} new_falling (queue [id])]
    (if (empty? new_falling)
      (disj falling id)
      (let [next_falling (peek new_falling)
            fresh_falling (->> (get supports next_falling)
                               (filter #(looses-support? %1 supported_by falling)))]
        (recur (into falling fresh_falling) (pop (reduce conj new_falling fresh_falling)))))))

(defn- falling_counts
  [ascending_blocks]
  (let [{supports :supports supported_by :supported_by} (drop_blocks ascending_blocks)
        ids (into #{} (map :id ascending_blocks))]
    (->> ids
         (map #(falling_blocks %1 supported_by supports))
         (map count)
         (into []))))

(defn solve_part2
  "Solves part 2 of day 22 of AdventOfCode 2023"
  [input]
  (let [ascending_blocks (indexed_blocks input)
        fallings (falling_counts ascending_blocks)]
    (str (reduce + fallings))))
