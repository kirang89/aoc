(ns aoc-2022.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-input [file]
  (->> file
       io/resource
       slurp
       str/split-lines))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
