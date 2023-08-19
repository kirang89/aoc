(ns aoc-2022.day-8
  (:require [clojure.string :as str]))

(def tree-map
  (->> "day_8_input.txt"
     aoc-2022.core/load-input
     (mapv (fn [l] (mapv #(Integer/parseInt %) (str/split l #""))))))

(def test-input [[6 6 7 9 1 4 7]
                 [1 8 2 3 4 9 8]
                 [9 9 1 6 4 2 1]
                 [7 8 3 7 5 8 2]])

(defn edge? [[row col] tree]
  (let [col-end  (dec (count (first tree)))
        last-row (dec (count tree))]
    (or (= 0 row)
        (= last-row row)
        (= 0 col)
        (= col-end col))))

(defn visible-from-pos? [target pos tree]
  (> (get-in tree target) (get-in tree pos)))

(defn visible-from-top? [[row col :as target] tree]
  (let [above (mapv (fn [r] [r col]) (range 0 row))]
    (every? #(visible-from-pos? target % tree) above)))

(defn visible-from-left? [[row col :as target] tree]
  (let [left (mapv (fn [c] [row c]) (range 0 col))]
    (every? #(visible-from-pos? target % tree) left)))

(defn visible-from-bottom? [[row col :as target] tree]
  (let [total-rows (count tree)
        bottom     (mapv (fn [r] [r col]) (range (inc row) total-rows))]
    (every? #(visible-from-pos? target % tree) bottom)))

(defn visible-from-right? [[row col :as target] tree]
  (let [cols (count (first tree))
        right (mapv (fn [c] [row c]) (range (inc col) cols))]
    (every? #(visible-from-pos? target % tree) right)))

(defn visible? [target tree]
  (if (edge? target tree)
    true
    (or
     (visible-from-top? target tree)
     (visible-from-left? target tree)
     (visible-from-bottom? target tree)
     (visible-from-right? target tree))))

(defn visible-trees [tree-map]
  (loop [idx 0 acc []]
    (if (> idx (count tree-map))
      (count acc)
      (let [row     (get tree-map idx)
            visible (keep-indexed (fn [col h]
                                    (when (visible? [idx col] tree-map)
                                      h))
                                  row)]
        (recur (inc idx) (into acc visible))))))


(visible-trees tree-map)
