(ns advent-of-code-2023.days.day13
  (:require [clojure.string :as str]))

(defn- reduce_bitmask_row
  [{x :x row_bit :row_bit column_bit :column_bit row_mask :row_mask column_masks :column_masks} item]
  {
   :x (inc x)
   :row_bit row_bit
   :column_bit (bit-shift-left column_bit 1)
   :row_mask (if (= "#" item) (+ row_mask column_bit) row_mask)
   :column_masks (if (= "#" item) (update column_masks x + row_bit) column_masks)
   })

(defn- reduce_bitmask_rows
  [{row_bit :row_bit row_masks :row_masks column_masks :column_masks} line]
  (let [items (str/split line #"")
        row_result (reduce
                     reduce_bitmask_row
                     {
                      :x 0
                      :row_bit row_bit
                      :column_bit 1
                      :row_mask 0
                      :column_masks column_masks
                      }
                     items)]
    {
     :row_bit (bit-shift-left row_bit 1)
     :row_masks (conj row_masks (:row_mask row_result))
     :column_masks (:column_masks row_result)
     }))

(defn- generate_bitmasks
  [lines]
  (let [line_length (count (first lines))]
    (reduce
      reduce_bitmask_rows
      {
       :row_bit 1
       :row_masks []
       :column_masks (into [] (repeat line_length 0))
       }
      lines)))

(defn- parse_part
  [part_text]
  (let [lines (str/split part_text #"\r?\n")]
    (->> lines
         (remove str/blank?)
         (generate_bitmasks)
         ((fn [{rows :row_masks columns :column_masks}] {:horizontal rows :vertical columns})))))

(defn parse_input
  "Parses the input for day 13 of AdventOfCode 2023"
  [input_string]
  (let [parts (str/split input_string #"\r?\n\r?\n")]
    (->> parts
         (remove str/blank?)
         (map parse_part)
         (into []))))

(defn- reflection_point?
  [items index]
  (let [length (count items)]
    (loop [left_index (dec index)
           right_index index]
      (if (or
            (< left_index 0)
            (>= right_index length))
        true
        (let [left_item (get items left_index)
              right_item (get items right_index)]
          (if (= left_item right_item)
            (recur (dec left_index) (inc right_index))
            false))))))

(defn- reflection_index
  [items disallowed]
  (let [length (count items)]
    (loop [index 1 previous_item (get items 0)]
      (if (= length index)
        0
        (let [item (get items index)]
          (if (and
                (not= index disallowed)
                (= item previous_item)
                (reflection_point? items index))
            index
            (recur (inc index) item)))))))

(defn- reflection_point_score
  [{horizontal :horizontal vertical :vertical}]
  (+ (reflection_index vertical 0) (* 100 (reflection_index horizontal 0))))

(defn solve_part1
  "Solves part 1 of day 13 of AdventOfCode 2023"
  [input]
  (->> input
       (map reflection_point_score)
       (reduce +)
       (str)))

(defn- smudge_new_reflection_index_on_level
  [items length smudge_level_bit known_reflection]
  (loop [smudge_index 0]
    (if (>= smudge_index length)
      0
      (let [smudged_items (update items smudge_index #(bit-xor %1 smudge_level_bit))
            reflection_point (reflection_index smudged_items known_reflection)]
        (if (> reflection_point 0)
          reflection_point
          (recur (inc smudge_index)))))))

(defn- smudge_new_reflection_index
  [items width]
  (let [length (count items)
        known_reflection (reflection_index items 0)]
    (loop [smudge_level 0
           smudge_level_bit 1]
      (if (>= smudge_level width)
        0
        (let [reflection_point (smudge_new_reflection_index_on_level items length smudge_level_bit known_reflection)]
          (if (not= reflection_point 0)
            reflection_point
            (recur (inc smudge_level) (bit-shift-left smudge_level_bit 1))))))))

(defn- smudge_reflection_point_score
  [{horizontal :horizontal vertical :vertical}]
  (+ (smudge_new_reflection_index vertical (count horizontal)) (* 100 (smudge_new_reflection_index horizontal (count vertical)))))

(defn solve_part2
  "Solves part 2 of day 13 of AdventOfCode 2023"
  [input]
  (->> input
       (map smudge_reflection_point_score)
       (reduce +)
       (str)))
