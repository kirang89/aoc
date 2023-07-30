(ns aoc-2022.day-5
  (:require [clojure.string :as str]))

(def input (aoc-2022.core/load-input "day_5_input.txt"))

(defn crate-position [row]
  (->> row
       (keep-indexed (fn [idx l]
                       (when (Character/isLetter (second l))
                         [(inc idx) [(second l)]])))
       (reduce (fn [m i] (into m [i])) {})))

(defn build-crate-stack [input]
  (->> input
       (take 8)
       (mapv #(partition 3 4 %))
       (mapv crate-position)
       reverse
       (apply merge-with into)))

(defn parse-instruction [str]
  (let [instruction-pattern #"move (\d+) from (\d) to (\d)"
        match-group         (drop 1 (re-matches instruction-pattern str))
        [idx from to]       (mapv #(Integer/parseInt %) match-group)]
    {:crates idx :from from :to to}))

(defn apply-instruction [cs {:keys [crates from to]}]
  (let [src-crates     (get cs from)
        crates-to-move (take-last crates src-crates)
        dest-crates    (get cs to)]
    (assoc cs
           from (vec (drop-last crates src-crates))
           to (into dest-crates (reverse crates-to-move)))))

;; Minor change for part 2
(defn apply-instruction-p2 [cs {:keys [crates from to]}]
  (let [src-crates     (get cs from)
        crates-to-move (take-last crates src-crates)
        dest-crates    (get cs to)]
    (assoc cs
           from (vec (drop-last crates src-crates))
           to (into dest-crates crates-to-move))))

(defn process-instructions [input]
  (let [crate-stack      (build-crate-stack input)
        instructions-str (drop 10 input)
        instructions     (mapv parse-instruction instructions-str)]
    (reduce apply-instruction
            crate-stack
            instructions)))

(defn topmost-crates [stack]
  (->> stack
       (map (fn [[k v]]
              [k (last v)]))
       (into {})))

(comment

  (def s (build-crate-stack input))
  (clojure.pprint/pprint s)

  ;; solution
  (clojure.pprint/pprint (process-instructions input))
  (clojure.pprint/pprint (topmost-crates (process-instructions input))))
