(ns aoc-2022.day2-p2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def shape-score
  {:shape/rock 1
   :shape/paper 2
   :shape/scissors 3})

(def outcome-score
  {:won  6
   :lost 0
   :draw 3})

(def opponent-moves
  {"A" :shape/rock
   "B" :shape/paper
   "C" :shape/scissors})

(def round-outcomes
  {"X" :lost
   "Y" :draw
   "Z" :won})

(def results
  {[:shape/rock :shape/rock]         :draw
   [:shape/rock :shape/paper]        :won
   [:shape/rock :shape/scissors]     :lost
   [:shape/paper :shape/rock]        :lost
   [:shape/paper :shape/paper]       :draw
   [:shape/paper :shape/scissors]    :won
   [:shape/scissors :shape/rock]     :won
   [:shape/scissors :shape/paper]    :lost
   [:shape/scissors :shape/scissors] :draw})

(defn load-rounds [file]
  (->> file
       aoc-2022.core/load-input
       (map (fn [l] (str/split l #" ")))))

(defn find-move [opponent-move round-outcome]
  (let [matching-play? (fn [[[om _] result]]
                         (and (= om opponent-move)
                              (= result (get round-outcomes round-outcome))))
        [[_ move] _]   (->> results (filter matching-play?) first)]
    move))

(defn calculate-total-score [data]
  (->> data
       (map (fn [[opp-move response-outcome]]
              (let [omove (get opponent-moves opp-move)]
                [omove (find-move omove response-outcome)])))
       (reduce (fn [acc [opp-move move :as round]]
                 (+ acc
                    (move shape-score)
                    (->> round
                         (get results)
                         (get outcome-score))))
               0)))

(comment
  (->> (load-rounds "day_2_input.txt")
       calculate-total-score))
