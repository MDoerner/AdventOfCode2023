(ns advent-of-code-2023.days.day20
  (:require [clojure.string :as str]))

(defn- parse_module
  [text]
  (let [main_parts (re-find #"(.+)\s+->\s+(.+)" text)
        module_name (get main_parts 1)
        type (cond
               (= module_name "button") :button
               (= module_name "broadcaster") :broadcast
               (str/starts-with? module_name "%") :flip-flop
               (str/starts-with? module_name "&") :conjunction)
        id (if (contains? #{"button" "broadcaster"} module_name)
             module_name
             (subs module_name 1))
        targets (->> (str/split (get main_parts 2) #", ")
                   (into []))]
    {:id id :type type :targets targets}))

(defn- parse_modules
  [text]
  (let [text_lines (str/split text #"\r?\n")
        button_line "button -> broadcaster"
        lines (concat [button_line] text_lines)]
    (->> lines
         (remove str/blank?)
         (map parse_module)
         (reduce #(assoc %1 (:id %2) %2) {}))))

(defn- start_state
  [modules]
  (-> modules
      (dissoc "button")
      (dissoc "broadcaster")
      (update-vals #(case (:type %1)
                      :flip-flop :off
                      :conjunction (let [id (:id %1)]
                                     (reduce
                                       (fn [in_states module]
                                         (if (some #{id} (:targets module))
                                           (assoc in_states (:id module) :low)
                                           in_states))
                                       {}
                                       (vals modules)))))))

(defn parse_input
  "Parses the input for day 20 of AdventOfCode 2023"
  [input_string]
  (let [modules (parse_modules input_string)
        state (start_state modules)]
    {:modules modules :state state}))

(defn- out_pulse_type
  [in_type module_type module_state]
  (case module_type
    :button :low
    :broadcast in_type
    :flip-flop (case in_type
                 :low (case module_state
                        :off :low
                        :on :high))
    :conjunction (if (every? #(= %1 :high) (vals module_state)) :low :high)))

(defn- round_of_pulses
  [modules initial_state]
  (loop [pulses [{:source "button" :target "broadcaster" :type :low}] lows 0 highs 0 state initial_state]
    (if (empty? pulses)
      {:lows lows :highs highs :state state}
      (let [[{source :source target :target pulse_type :type} & remaining] pulses
            new_lows (if (= pulse_type :low) (inc lows) lows)
            new_highs (if (= pulse_type :high) (inc highs) highs)]
        (if (not (contains? modules target))
          (recur remaining new_lows new_highs state)
          (let [{module_type :type targets :targets} (get modules target)
                new_state (case module_type
                            :button state
                            :broadcast state
                            :flip-flop (case pulse_type
                                         :high state
                                         :low (update state target #(case %1
                                                                      :off :on
                                                                      :on :off)))
                            :conjunction (update state target #(assoc %1 source pulse_type)))
                new_module_state (get new_state target)
                fresh_pulses (if (and (= module_type :flip-flop) (= pulse_type :high))
                               []
                               (map (fn [t] {:source target :target t :type (out_pulse_type pulse_type module_type new_module_state)}) targets))]
            (recur (concat remaining fresh_pulses) new_lows new_highs new_state)))))))

(defn- rounds_of_pulses
  [n modules initial_state]
  (loop [m n lows 0 highs 0 state initial_state]
    (if (= m 0)
      {:lows lows :highs highs :state state}
      (if (and (= initial_state state) (not= n m))
        (let [cycle_length (- n m)
              full_cycle_count (int (/ n cycle_length))
              remaining_rounds (mod n full_cycle_count)
              {r_highs :highs r_lows :lows r_state :state} (rounds_of_pulses remaining_rounds modules initial_state)]
          {
           :lows (+ r_lows (* full_cycle_count lows))
           :highs (+ r_highs (* full_cycle_count highs))
           :state r_state
           })
        (let [{r_highs :highs r_lows :lows r_state :state} (round_of_pulses modules state)]
          (recur (dec m) (+ lows r_lows) (+ highs r_highs) r_state))))))


(defn solve_part1
  "Solves part 1 of day 20 of AdventOfCode 2023"
  [input]
  (let [{modules :modules initial_state :state} input
        rounds 1000
        {highs :highs lows :lows} (rounds_of_pulses rounds modules initial_state)]
    (str (* highs lows))))

(defn- search_round_of_pulses
  [modules initial_state destination]
  (loop [pulses [{:source "button" :target "broadcaster" :type :low}] hits 0 state initial_state]
    (if (empty? pulses)
      {:hits hits :state state}
      (let [[{source :source target :target pulse_type :type} & remaining] pulses
            new_hits (if (and (= target destination) (= pulse_type :low)) (inc hits) hits)]
        (if (not (contains? modules target))
          (recur remaining new_hits state)
          (let [{module_type :type targets :targets} (get modules target)
                new_state (case module_type
                            :button state
                            :broadcast state
                            :flip-flop (case pulse_type
                                         :high state
                                         :low (update state target #(case %1
                                                                      :off :on
                                                                      :on :off)))
                            :conjunction (update state target #(assoc %1 source pulse_type)))
                new_module_state (get new_state target)
                fresh_pulses (if (and (= module_type :flip-flop) (= pulse_type :high))
                               []
                               (map (fn [t] {:source target :target t :type (out_pulse_type pulse_type module_type new_module_state)}) targets))]
            (recur (concat remaining fresh_pulses) new_hits new_state)))))))

(defn- rounds_till_reached                                  ;too naive
  [modules initial_state destination]
  (loop [m 1 state initial_state]
    (let [{hits :hits new_state :state} (search_round_of_pulses modules state destination)]
      (if (= hits 1)
        m
        (recur (inc m) new_state)))))

(defn- edge_list_string
  [modules]
  (->> modules
       (vals)
       (map (fn [{id :id type :type targets :targets}] (into [] (map-indexed (fn [index target] [id type target index]) targets))))
       (reduce into [])
       (map #(str/join "\t" %1))
       (str/join "\n")))

(defn- node_list_string
  [modules]
  (let [non_end_modules (->> modules
                             (vals)
                             (map (fn [{id :id type :type}] [id type])))
        end_modules (->> modules
                         (vals)
                         (map :targets)
                         (reduce into #{})
                         (filter #(not (contains? modules %1)))
                         (map (fn [id] [id :none])))]
    (->> (concat non_end_modules end_modules)
         (map #(str/join "\t" %1))
         (str/join "\n"))))


; The solution of the problem is based on an analysis of the graph performed on a visual representation.
; Each module the broadcaster sends pulses to is the start of a sequence of 12 flip-flop modules feeding into one conjunction module.
; Each consecutive node fires half as frequently as the prior one. The index of the modules connecting to the conjunction node
; determine the frequency at which this one fires and then resets the entire sequence.
; There are inverters right after the four conjunction modules, which then lead into a further conjunction module connected to the end node.
; This means that the end is only reached with a single low pulse whenever the frequencies of all four parts align.

(defn- activation_frequency
  [modules start_module]
  (loop [current_bit 1 freq 0 module start_module]
    (let [{targets :targets} (get modules module)
          flip-flop-targets (filter #(= :flip-flop (:type (get modules %1))) targets)]
      (if (empty? flip-flop-targets)
        (+ freq current_bit)
        (if (= (count targets) 2)
          (recur (bit-shift-left current_bit 1) (+ freq current_bit) (first flip-flop-targets))
          (recur (bit-shift-left current_bit 1) freq (first flip-flop-targets))))
      )))

(defn- activation_frequencies
  [modules]
  (let [starts (:targets (get modules "broadcaster"))]
    (->> starts
         (map #(activation_frequency modules %1))
         (into #{}))))

(defn- gcd
  "Greatest Common Divider via Euclidean Algorithm"
  [n m]
  (if (< n m)
    (gcd m n)
    (loop [a n b m]
      (if (= b 0)
        a
        (recur b (mod a b))))))

(defn- lcm
  "Least Common Multiple via Greatest Common Divider"
  ([n m]
   (/ (* n m) (gcd n m)))
  ([numbers]
   (reduce lcm 1 numbers)))

(defn- smart_rounds_till_reached
  [modules]
  (let [freqs (activation_frequencies modules)]
    (lcm freqs)))

(defn solve_part2
  "Solves part 2 of day 20 of AdventOfCode 2023"
  [input]
  (let [{modules :modules} input]
    (str (smart_rounds_till_reached modules))))
