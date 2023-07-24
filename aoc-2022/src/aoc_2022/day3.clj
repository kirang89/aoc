(ns aoc-2022.day3)

(defonce priority-map
  (merge (zipmap (map char (range 97 123)) (range 1 27))
       (zipmap (map char (range 65 91)) (range 27 53))))

(defn split-by-compartment [rucksacks]
  (map (fn [s]
         (let [items (count s)]
           [(subs s 0 (/ items 2))
            (subs s (/ items 2) items)]))
       rucksacks))

(defn common-items [comps]
  (->> comps
       (map set)
       (apply clojure.set/intersection)))

(defn priority-sum [data]
  (->> data
       (map common-items)
       (map (fn [i] (get priority-map (first i))))
       (reduce +)))

(defonce input (aoc-2022.core/load-input "day_3_input.txt"))

(comment
  ;; part 1
  (->> input split-by-compartment priority-sum)

  ;; part 2
  (->> input (partition 3) priority-sum))
