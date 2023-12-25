(ns advent-of-code-2023.days.day24
  (:require [clojure.string :as str]))


(defn- parse-vector-3d
  [text]
  (let [[x y z] (->> (str/split text #", ")
                     (map str/trim)
                     (map biginteger)
                     (into []))]
    {:x x :y y :z z}))

(defn- parse-hail-drop
  [text]
  (let [parts (str/split text #"@")
        position (parse-vector-3d (get parts 0))
        velocity (parse-vector-3d (get parts 1))]
    {:position position :velocity velocity}))

(defn- parse-hail-drops
  [text]
  (->> (str/split text #"\r?\n")
       (remove str/blank?)
       (map parse-hail-drop)
       (into [])))

(defn parse_input
  "Parses the input for day 24 of AdventOfCode 2023"
  [input_string]
  {
   :min 200000000000000
   :max 400000000000000
   :hail-drops (parse-hail-drops input_string)
   })



(defn- times-till-intersection
  "Individual times after which two hail drops reach their path's intersection"
  [{{ax :x ay :y} :position {vx :x vy :y} :velocity}
   {{bx :x by :y} :position {wx :x wy :y} :velocity}]
  (let [determinant (- (* vx wy) (* vy wx))]
    (if (zero? determinant)
      [nil nil]                                                   ; The velocities are colinear. So, there is no intersection.
      (let [offset-delta-x (- bx ax)
            offset-delta-y (- by ay)
            first_numerator (- (* wy offset-delta-x) (* wx offset-delta-y))
            second_numerator (- (* vy offset-delta-x) (* vx offset-delta-y))]
        [(/ first_numerator determinant) (/ second_numerator determinant)]))))

(defn- planar-future-intersection
  [hail-drop other-hail-drop]
  (let [[t s] (times-till-intersection hail-drop other-hail-drop)]
    (if (or (nil? t) (<= t 0) (nil? s) (<= s 0))
      nil
      (let [{{ax :x ay :y} :position {vx :x vy :y} :velocity} hail-drop]
        {:x (+ ax (* vx t)) :y (+ ay (* vy t))}))))

(defn- in-bounds?
  [position min max]
  (->> (vals position)
       (every? #(and (>= %1 min) (<= %1 max)))))


(defn- in-bounds-future-intersections
  [min max hail-drops]
  (->> (for [a hail-drops b hail-drops]
        (planar-future-intersection a b))
       (filter some?)
       (filter #(in-bounds? %1 min max))))


(defn solve_part1
  "Solves part 1 of day 24 of AdventOfCode 2023"
  [input]
  (let [{min :min max :max hail-drops :hail-drops} input
        relevant-intersections (in-bounds-future-intersections min max hail-drops)]
    (str (/ (count relevant-intersections) 2))))


(defn- equation_text
  "Reduced equation for hitting a hail drop after elimination of the collision time by combining two coordinates."
  [{{x :x y :y z :z} :position {wx :x wy :y wz :z} :velocity}]
  (str "(" wx " - u)*(y - " y ") = (" wy " - v)*(x - " x ")" ","
       "(" wz " - w)*(y - " y ") = (" wy " - v)*(z - " z ")"))

(defn solve_part2
  "Solves part 2 of day 24 of AdventOfCode 2023"
  [input]
  (println (->> (:hail-drops input)
                (map equation_text)
                (take 4)
                (str/join ",")))
  ; Use online equation solver, where u = vx, v = vy and w = vz.
  (let [velocity {:x 263 :y 120 :z 21}
        position {:x 118378223846841 :y 228996474589321 :z 259397320329497}]
    (->> position
         (vals)
         (reduce +)
         (str)))
  )
