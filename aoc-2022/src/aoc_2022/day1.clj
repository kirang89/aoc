(ns aoc-2022.day1
  (:require [clojure.java.io :as io]))

(def input (slurp (io/resource "day_1_input.txt")))

(def calorie-count
  (->> (clojure.string/split input #"\n\n")
       (map (fn [elf-cal-str]
              (->> (clojure.string/split elf-cal-str #"\n")
                   (map #(Integer/parseInt %))
                   (reduce +))))))

;; Alternate Implementation
(comment
  (def calorie-count-alt
    (with-open [rdr (io/reader (io/resource "day_1_input.txt"))]
      (->> rdr
           line-seq
           (reduce conj [])
           (partition-by #(if (empty? %) true false))
           (filter #(not (empty? (first %))))
           (map (fn [coll]
                  (->> coll
                       (map #(Integer/parseInt %))
                       (reduce +)))))))
  (apply max calorie-count-alt))


(def max-calorie-count (apply max calorie-count))

(def sum-top-three-calorie (->> calorie-count
                               (sort >)
                               (take 3)
                               (reduce +)))
