(ns aoc-2022.day-6)

(defonce datastream
  (first (aoc-2022.core/load-input "day_6_input.txt")))

(defn first-match-start [match-len]
  (reduce (fn [idx char]
            (let [marker-uniq-chars (-> datastream
                                        (subs idx (+ idx match-len))
                                        set)]
              (if (= match-len (count marker-uniq-chars))
                (reduced (+ idx match-len))
                (inc idx))))
          0
          datastream))

(def start-packet-marker-len 4)
(def start-message-marker-len 14)

(def first-packet-start (first-match-start start-packet-marker-len))
(def first-message-start (first-match-start start-message-marker-len))
