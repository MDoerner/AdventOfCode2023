(ns advent-of-code-2023.days.day19
  (:require [clojure.string :as str]))

(defn- str->destination
  [text]
  (case text
    "R" :rejected
    "A" :accepted
    text))

(defn- parse_test
  [text]
  (let [parsed (re-find #"([axsm])([<>])(\d+):(.+)" text)
        variable (keyword (get parsed 1))
        operator (get parsed 2)
        value (Integer/parseInt (get parsed 3))
        result (str->destination (get parsed 4))]
    {:variable variable :operator operator :value value :result result}))

(defn- parse_workflow
  [text]
  (let [main_parts (re-find #"(.+)\{(.+)\}" text)
        id (get main_parts 1)
        rules (str/split (get main_parts 2) #",")
        default (str->destination (last rules))
        tests (->> rules
                   (filter #(str/includes? %1 ":"))
                   (map parse_test)
                   (into []))]
    {:id id :tests tests :default default}))

(defn- parse_workflows
  [text]
  (let [lines (str/split text #"\r?\n")]
    (->> lines
         (remove str/blank?)
         (map parse_workflow)
         (into []))))



(defn- parse_part
  [text]
  (let [parsed (re-find #"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}" text)
        x (Integer/parseInt (get parsed 1))
        m (Integer/parseInt (get parsed 2))
        a (Integer/parseInt (get parsed 3))
        s (Integer/parseInt (get parsed 4))
        ]
    {:x x :m m :a a :s s}))

(defn- parse_parts
  [text]
  (let [lines (str/split text #"\r?\n")]
    (->> lines
         (remove str/blank?)
         (map parse_part)
         (into []))))


(defn parse_input
  "Parses the input for day 19 of AdventOfCode 2023"
  [input_string]
  (let [blocks (str/split input_string #"\r?\n\r?\n")
        workflows (parse_workflows (first blocks))
        parts (parse_parts (last blocks))]
    {:workflows workflows :parts parts}))


(defn- test->fn
  [{op :operator variable :variable val :value result :result}]
  (fn [part]
    (if ((case op "<" < ">" >) (variable part) val)
      result
      nil)))

(defn- workflow->fn
  [{tests :tests default :default}]
  (fn [part]
    (let [result_fns (map test->fn tests)
          results (map #(%1 part) result_fns)
          positive_results (drop-while nil? results)]
      (if (empty? positive_results)
        default
        (first positive_results)))))

(defn- workflow_fns
  [workflows]
  (reduce #(assoc %1 (:id %2) (workflow->fn %2)) {} workflows))

(defn- evaluate_part
  [part flow_fns start_workflow]
   (loop [flow start_workflow]
    (let [flow_fn (get flow_fns flow)
          result (flow_fn part)]
      (if (contains? #{:accepted :rejected} result)
        result
        (recur result)))))

(defn- part_score
  [part]
  (reduce + (vals part)))

(defn solve_part1
  "Solves part 1 of day 19 of AdventOfCode 2023"
  [input]
  (let [flow_fns (workflow_fns (:workflows input))
        start_flow "in"]
    (->> (:parts input)
        (filter #(= :accepted (evaluate_part %1 flow_fns start_flow)))
         (map part_score)
         (reduce +)
         (str))))

(def accepted_boxes)

(defn- accepted_boxes_for_test
  [workflows {op :operator variable :variable val :value result :result} box]
  (if (or (= result :rejected) (nil? box))
    []
    (let [{lc :lower_corner uc :upper_corner} box
          condition_box (case op
                          ">" (if (>= val (variable uc))
                                nil
                                (let [lower (assoc lc variable (max (variable lc) (inc val)))]
                                  {:lower_corner lower :upper_corner uc}))
                          "<" (if (<= val (variable lc))
                                nil
                                (let [upper (assoc uc variable (min (variable uc) (dec val)))]
                                  {:lower_corner lc :upper_corner upper})))]
      (if (nil? condition_box)
        []
        (if (= result :accepted)
          [condition_box]
          (let [workflow (get workflows result)]
            (accepted_boxes workflows workflow condition_box)))))))

(defn- remaining_box
  [box {op :operator variable :variable val :value}]
  (if (nil? box)
     nil
     (let [{lc :lower_corner uc :upper_corner} box]
       (case op
         "<" (if (> val (variable uc))
               nil
               (let [lower (assoc lc variable (max (variable lc) val))]
                 {:lower_corner lower :upper_corner uc}))
         ">" (if (< val (variable lc))
               nil
               (let [upper (assoc uc variable (min (variable uc) val))]
                 {:lower_corner lc :upper_corner upper}))))))

(defn- accepted_boxes
  [workflows {tests :tests default :default id :id} condition_box]
  (let [[test_boxes final_box] (reduce (fn [[boxes rest] test]
                             [(into boxes (accepted_boxes_for_test workflows test rest))
                              (remaining_box rest test)])
                           [[] condition_box]
                           tests)
        default_boxes (case default
                      :accepted [final_box]
                      :rejected []
                      (let [workflow (get workflows default)]
                        (accepted_boxes workflows workflow final_box)))]
    (into test_boxes default_boxes)))

(defn- box_volume
  [{lc :lower_corner uc :upper_corner}]
  (->> (merge-with - (update-vals uc inc) lc)
      (vals)
      (map biginteger)
       (reduce *)))

(defn- workflow_dict
  [workflows]
  (reduce #(assoc %1 (:id %2) %2) {} workflows))

(defn- accepteds_count
  [workflows start_id start_box]
  (let [flows (workflow_dict workflows)
        start_flow (get flows start_id)
        boxes (accepted_boxes flows start_flow start_box)]
    (->> boxes
         (map box_volume)
         (reduce +))))

(defn solve_part2
  "Solves part 2 of day 19 of AdventOfCode 2023"
  [input]
  (let [start_box {:lower_corner {:x 1 :m 1 :a 1 :s 1} :upper_corner {:x 4000 :m 4000 :a 4000 :s 4000}}
        start_id "in"]
    (-> input
        (:workflows)
        (accepteds_count start_id start_box)
        (str))))
