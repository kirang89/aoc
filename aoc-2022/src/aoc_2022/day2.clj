(ns aoc-2022.day2
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

(def opponent-set
  {"A" :shape/rock
   "B" :shape/paper
   "C" :shape/scissors})

(def response-set
  {"X" :shape/rock
   "Y" :shape/paper
   "Z" :shape/scissors})

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
       io/resource
       slurp
       str/split-lines
       (map (fn [l] (str/split l #" ")))))

(defn calculate-total-score [data]
  (->> data
       (map (fn [[opp-move move]]
              [(get opponent-set opp-move) (get response-set move)]))
       (reduce (fn [acc [opp-move move :as round]]
                 (+ acc
                    (move shape-score)
                    (->> round
                         (get results)
                         (get outcome-score))))
               0)))

(calculate-total-score (load-rounds "day_2_input.txt"))
