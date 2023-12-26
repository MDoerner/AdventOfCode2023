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

(defn solve_part2_naive
  "Solves part 2 of day 21 of AdventOfCode 2023"
  [input]
  (let [{steps :steps2 start :start width :width height :height rocks :rocks} input
        reachable (move_looping rocks width height start steps)]
    (println (str "single odd covered: " (->> reachable
                                              (filter (fn [{x :x y :y}]
                                                        (and (< x 131) (< y 131) (>= x 0) (>= y 0))))
                                              (count))))
    (println (str "single even covered: " (->> reachable
                                               (filter (fn [{x :x y :y}]
                                                         (and (< x 0) (< y 131) (>= x -131) (>= y 0))))
                                               (count))))
    (println (str "all covered: " (->> reachable
                                       (filter (fn [{x :x y :y}]
                                                 (<= (+ (abs (- x 65)) (abs (- y 65))) 458)))
                                       (count))))
    (println (str "left tip: " (->> reachable
                                    (filter (fn [{x :x y :y}]
                                              (and (< x -393) (< y 131) (>= y 0))))
                                    (count))))
    (println (str "right tip: " (->> reachable
                                     (filter (fn [{x :x y :y}]
                                               (and (>= x 524) (< y 131) (>= y 0))))
                                     (count))))
    (println (str "top tip: " (->> reachable
                                   (filter (fn [{x :x y :y}]
                                             (and (< y -393) (< x 131) (>= x 0))))
                                   (count))))
    (println (str "bottom tip: " (->> reachable
                                      (filter (fn [{x :x y :y}]
                                                (and (>= y 524) (< x 131) (>= x 0))))
                                      (count))))
    (println (str "small top-right corner: " (->> reachable
                                                  (filter (fn [{x :x y :y}]
                                                            (and (>= x 524) (< y 0))))
                                                  (count))))
    (println (str "small bottom-right corner: " (->> reachable
                                                     (filter (fn [{x :x y :y}]
                                                               (and (>= x 524) (>= y 131))))
                                                     (count))))
    (println (str "small top-left corner: " (->> reachable
                                                 (filter (fn [{x :x y :y}]
                                                           (and (< x -393) (< y 0))))
                                                 (count))))
    (println (str "small bottom-left corner: " (->> reachable
                                                    (filter (fn [{x :x y :y}]
                                                              (and (< x -393) (>= y 131))))
                                                    (count))))
    (println (str "big top-right corner: " (->> reachable
                                                (filter (fn [{x :x y :y}]
                                                          (and (< x 524) (>= x 393) (< y 0) (>= y -131))))
                                                (count))))
    (println (str "big bottom-right corner: " (->> reachable
                                                   (filter (fn [{x :x y :y}]
                                                             (and (< x 524) (>= x 393) (< y 262) (>= y 131))))
                                                   (count))))
    (println (str "big top-left corner: " (->> reachable
                                               (filter (fn [{x :x y :y}]
                                                         (and (>= x -393) (< x -262) (< y 0) (>= y -131))))
                                               (count))))
    (println (str "big bottom-left corner: " (->> reachable
                                                  (filter (fn [{x :x y :y}]
                                                            (and (>= x -393) (< x -262) (< y 262) (>= y 131))))
                                                  (count))))
    (str (count reachable))
    ))



; The solution below relies on special properties of the input.
; 1. The outpost points of the garden do not contain rocks.
; 2. The start is in the garden's center.
; 3. There are no rocks in a straight line between the start and the edges. (Not satisfied in example input)
; 4. Height = Weight = 2 * (2k + 1) + 1
; 5. The rocks are distributed so sparsely, that the farthest point from any corner is the opposit corner.
; 6. Steps is odd and larger than 2 * Height.
;
; As a consequence, for every copy of the garden, from any point therein there is a shortest path to the center that passes
; through the middle of the edge closest to the center, if the copy in a straight line above, below, left or right from the
; original garden, and through the closest corner, otherwise.

(defn- points_within_distance_even
  [rocks width height point-set]
  (loop [reachable point-set barely_reachable point-set within_distance {0 1} distance 0]
    (if (empty? barely_reachable)
      within_distance
      (let [new_barely_reachable(->> barely_reachable
                                     (map #(move_twice rocks width height %1))
                                     (reduce into #{})
                                     (filter #(not (contains? reachable %1)))
                                     (into #{}))
            new_reachable (into reachable new_barely_reachable)
            new_within_distance (assoc within_distance distance (count reachable))]
        (recur new_reachable new_barely_reachable new_within_distance (+ 2 distance))))))

(defn- points_within_distance
  [rocks width height point]
  (let [even-distances (points_within_distance_even rocks width height #{point})
        moved_once (into #{} (move_once rocks width height point))
        odd-distance-minus-one (points_within_distance_even rocks width height moved_once)]
    (reduce (fn [distance-counts [distance-minus-one reachable-count]]
              (assoc distance-counts (inc distance-minus-one) reachable-count))
            even-distances
            odd-distance-minus-one)))


(defn- partial_cover_distances
  [max_distance extent steps direction]
  (case direction
    :center (if (> steps max_distance) [] [{:kind :even :distance steps :count 1}])
    (:left :right :up :down) (let [to_first (/ (inc extent) 2)
                                   first_to_last_possible (- steps (+ max_distance to_first))]
                               (if (> first_to_last_possible 0)
                                 (let [full_garden_distance (inc (int (/ first_to_last_possible extent)))
                                       last_even? (odd? full_garden_distance)
                                       distance_to_first_partial (+ (* full_garden_distance extent) to_first)
                                       distances (range distance_to_first_partial (inc steps) extent)
                                       remaining_distances (map-indexed (fn [index dist]
                                                                          {
                                                                           :distance (- steps dist)
                                                                           :count 1
                                                                           :kind (if last_even?
                                                                                   (if (even? index) :odd :even)
                                                                                   (if (odd? index) :odd :even))
                                                                           }) distances)]
                                   (into [] remaining_distances))
                                 {:even 0 :odd 0})
                               )
    (:top-left :top-right :bottom-left :bottom-right) (let [to_first (inc extent)
                                                            first_to_last_possible (- steps (+ max_distance to_first))]
                                                        (if (> first_to_last_possible 0)
                                                          (let [full_garden_distance (inc (int (/ first_to_last_possible extent)))
                                                                last_even? (even? full_garden_distance)
                                                                distance_to_first_partial (+ (* full_garden_distance extent) to_first)
                                                                distances (range distance_to_first_partial (inc steps) extent)
                                                                remaining_distances (map-indexed (fn [index dist]
                                                                                                   {
                                                                                                    :distance (- steps dist)
                                                                                                    :count (+ full_garden_distance (inc index))
                                                                                                    :kind (if last_even?
                                                                                                            (if (even? index) :odd :even)
                                                                                                            (if (odd? index) :odd :even))
                                                                                                    }) distances)]
                                                            (into [] remaining_distances))
                                                          {:even 0 :odd 0}))))

(defn- reachable_count
  [rocks width height start steps]
  (let [reference_points {
                          :center start
                          :left (assoc start :x (dec width))
                          :right (assoc start :x 0)
                          :up (assoc start :y (dec height))
                          :down (assoc start :y 0)
                          :top-left {:x (dec width) :y (dec height)}
                          :top-right {:x 0 :y (dec height)}
                          :bottom-left {:x (dec width) :y 0}
                          :bottom-right {:x 0 :y 0}
                          }
        points_in_distance (update-vals reference_points #(points_within_distance rocks width height %1))
        max_distances (->> points_in_distance
                           (map (fn [[direction counts_in_distance]]
                                  [direction (apply max (keys counts_in_distance))]))
                           (into {}))
        full_cover_points {
                           :even (->> (get points_in_distance :center)
                                      (filter (fn [[distance _]] (even? distance)))
                                      (map second)
                                      (apply max))
                           :odd (->> (get points_in_distance :center)
                                     (filter (fn [[distance _]] (odd? distance)))
                                     (map second)
                                     (apply max))
                           }
        size height
        chunks (/ (- steps (/ (dec size) 2)) size)
        full_cover_counts {
                            :even (let [border-length (if (even? chunks) chunks (dec chunks))]
                                    (* border-length border-length))
                            :odd (let [border-length (if (odd? chunks) chunks (dec chunks))]
                                   (* border-length border-length))
                           }
        full_cover_reachable (->> full_cover_counts
                                  (merge-with * full_cover_points)
                                  (vals)
                                  (reduce +))
        remaining_distances (->> max_distances
                                 (map (fn [[direction max_distance]] [direction (partial_cover_distances max_distance height steps direction)]))
                                 (into {}))
        remaining_reachable (->> remaining_distances
                                 (map (fn [[direction specs]]
                                        (let [distance-counts (get points_in_distance direction)]
                                          (map (fn [{distance :distance times :count}] (* times (get distance-counts distance))) specs))))
                                 (reduce into [])
                                 (reduce +))
        ]
    ;(println start)
    ;(println chunks)
    ;(println " ")
    ;(println (str "single odd covered: " (:odd full_cover_points)))
    ;(println (str "single even covered: " (:even full_cover_points)))
    ;(println (str "odd covered count: " (:odd full_cover_counts)))
    ;(println (str "even covered count: " (:even full_cover_counts)))
    ;(println (str "all covered: " full_cover_reachable))
    ;(println (str "left tip: " (get remaining_distances :left)))
    ;(println (str "right tip: " (get remaining_distances :right)))
    ;(println (str "top tip: " (get remaining_distances :up)))
    ;(println (str "bottom tip: " (get remaining_distances :down)))
    ;(println (str "top-right corner specs: " (get remaining_distances :top-left)))
    ;(println (str "bottom-right corner specs: " (get remaining_distances :bottom-left)))
    ;(println (str "top-left corner specs: " (get remaining_distances :top-right)))
    ;(println (str "bottom-left corner specs: " (get remaining_distances :bottom-right)))
    ;(println (str "left tip: " (get (get points_in_distance :left) 130)))
    ;(println (str "left tip direct: " (count (move rocks width height (get reference_points :left) 130))))
    ;(println (str "right tip: " (get (get points_in_distance :right) 130)))
    ;(println (str "top tip: " (get (get points_in_distance :up) 130)))
    ;(println (str "bottom tip: " (get (get points_in_distance :down) 130)))
    ;(println (str "small top-right corner: " (get (get points_in_distance :top-left) 64)))
    ;(println (str "small bottom-right corner: " (get (get points_in_distance :bottom-left) 64)))
    ;(println (str "small top-left corner: " (get (get points_in_distance :top-right) 64)))
    ;(println (str "small bottom-left corner: " (get (get points_in_distance :bottom-right) 64)))
    ;(println (str "big top-right corner: " (get (get points_in_distance :top-left) 195)))
    ;(println (str "big bottom-right corner: " (get (get points_in_distance :bottom-left) 195)))
    ;(println (str "big top-left corner: " (get (get points_in_distance :top-right) 195)))
    ;(println (str "big bottom-left corner: " (get (get points_in_distance :bottom-right) 195)))
    ;(println " ")
    ;(println (+ full_cover_reachable remaining_reachable))
    (+ full_cover_reachable remaining_reachable)))

(defn solve_part2
  "Solves part 2 of day 21 of AdventOfCode 2023"
  [input]
  (let [{steps :steps2 start :start width :width height :height rocks :rocks} input]
    ;(println [width height])
    ;(println (reachable_count rocks width height start 589))
    ;(println (time (solve_part2_naive (assoc input :steps2 589))))
    (str (reachable_count rocks width height start steps))
    ))
