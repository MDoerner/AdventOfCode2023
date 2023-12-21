(ns advent-of-code-2023.days.day21
  (:require [clojure.string :as str]))

(defn- parse_line_of_rocks
  [y text]
  (let [positions (str/split text #"")]
    (->> positions
         (map-indexed #(if (contains? #{"#" "S"} %2)
                         (let [point {:x %1 :y y}
                               type (case %2
                                      "#" :rock
                                      "S" :start)]
                           {:point point :type type})))
         (filter some?)
         (reduce (fn [rocks_and_start {point :point type :type}]
                   (case type
                     :start (assoc rocks_and_start :start point)
                     :rock (update rocks_and_start :rocks #(conj %1 point))))
                 {:rocks #{}}))))

(defn- parse_rocks
  [lines]
  (->> lines
       (map-indexed parse_line_of_rocks)
       (reduce (fn [rocks_and_start {new_rocks :rocks new_start :start}]
                 (let [with_new_rocks (update rocks_and_start :rocks #(into %1 new_rocks))]
                   (if (nil? new_start)
                     with_new_rocks
                     (assoc with_new_rocks :start new_start))))
               {:rocks #{}})))

(defn parse_input
  "Parses the input for day 21 of AdventOfCode 2023"
  [input_string]
  (let [lines (remove str/blank? (str/split input_string #"\r?\n"))
        {rocks :rocks start :start} (parse_rocks lines)
        height (count lines)
        widths (count (first lines))]
    {
     :steps1 64
     :steps2 26501365
     :start start
     :width widths
     :height height
     :rocks rocks
     }
    ))

(defn- add_positions
  [pos other_pos]
  (merge-with + pos other_pos))

(defn- in_bounds?
  [width height point]
  (let [{x :x y :y} point]
    (and (>= x 0) (>= y 0) (< x width) (< y height))))

(defn- move_once
  [rocks width height point]
  (->> [{:x 0 :y -1} {:x 0 :y 1} {:x 1 :y 0} {:x -1 :y 0}]
       (map #(add_positions point %1))
       (filter #(and (not (contains? rocks %1))
                     (in_bounds? width height %1)))))

(defn- move_twice
  [rocks width height point]
  (->> point
       (move_once rocks width height)
       (map #(move_once rocks width height %1))
       (reduce into #{})))

(defn- move
  [rocks width height point n]
  (if (= n 1)
    (move_once rocks width height point)
    (if (not= (mod n 2) 0)
      (let [one_less_to_move (move_once rocks width height point)]
        (->> one_less_to_move
             (map #(move rocks width height %1 (dec n)))
             (reduce into #{})))
      (loop [reachable #{point} barely_reachable #{point} m (/ n 2)]
        (if (= m 0)
          reachable
          (let [new_barely_reachable(->> barely_reachable
                                   (map #(move_twice rocks width height %1))
                                   (reduce into #{})
                                   (filter #(not (contains? reachable %1)))
                                   (into #{}))
              new_reachable (into reachable new_barely_reachable)]
            (recur new_reachable new_barely_reachable (dec m))))))))



(defn solve_part1
  "Solves part 1 of day 21 of AdventOfCode 2023"
  [input]
  (let [{steps :steps1 start :start width :width height :height rocks :rocks} input]
    (str (count (move rocks width height start steps)))))


(defn- map_point
  [width height point]
  (if (in_bounds? width height point)
    point
    (let [{x :x y :y} point]
      {:x (mod x width) :y (mod y height)})))

(defn- move_once_looping
  [rocks width height point]
  (->> [{:x 0 :y -1} {:x 0 :y 1} {:x 1 :y 0} {:x -1 :y 0}]
       (map #(add_positions point %1))
       (filter #(not (contains? rocks (map_point width height %1))))))

(defn- move_twice_looping
  [rocks width height point]
  (->> point
       (move_once_looping rocks width height)
       (map #(move_once_looping rocks width height %1))
       (reduce into #{})))

(defn- move_looping
  [rocks width height point n]
  (if (= n 1)
    (move_once_looping rocks width height point)
    (if (not= (mod n 2) 0)
      (let [one_less_to_move (move_once_looping rocks width height point)]
        (->> one_less_to_move
             (map #(move_looping rocks width height %1 (dec n)))
             (reduce into #{})))
      (loop [reachable #{point} barely_reachable #{point} m (/ n 2)]
        (if (= m 0)
          reachable
          (let [new_barely_reachable(->> barely_reachable
                                         (map #(move_twice_looping rocks width height %1))
                                         (reduce into #{})
                                         (filter #(not (contains? reachable %1)))
                                         (into #{}))
                new_reachable (into reachable new_barely_reachable)]
            (recur new_reachable new_barely_reachable (dec m))))))))

(defn solve_part2
  "Solves part 2 of day 21 of AdventOfCode 2023"
  [input]
  (let [{steps :steps2 start :start width :width height :height rocks :rocks} input]
    ;(println (move_twice rocks width height {:x 0 :y 4}))
    ;(println (move_once_looping rocks width height {:x -1 :y 4}))
    (str (count (move_looping rocks width height start steps)))
    ))
