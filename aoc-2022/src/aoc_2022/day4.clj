(ns aoc-2022.day4
  (:require [clojure.string :as str]))

(defn rangeify [rs]
  (reduce
   (fn [acc r]
     (->> (str/split r #"-")
          (mapv #(Integer/parseInt %))
          (conj acc)))
   []
   rs))

(defn contains-full-range? [[[s1 e1] [s2 e2]]]
  (or (and (>= s1 s2) (<= e1 e2))
      (and (>= s2 s1) (<= e2 e1))))

(defn contains-overlapping-range? [[[s1 e1] [s2 e2]]]
  (or (and (>= s1 s2) (<= s1 e2))
      (and (>= s2 s1) (<= s2 e1))))

(defn count-full-ranges [input]
  (->> input
       (mapv #(str/split % #","))
       (mapv rangeify)
       (filter contains-full-range?)
       count))

(defn count-overlapping-ranges [input]
  (->> input
       (mapv #(str/split % #","))
       (mapv rangeify)
       (filter contains-overlapping-range?)
       count))

(->> "day_4_input.txt" aoc-2022.core/load-input count-overlapping-ranges)
