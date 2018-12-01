(ns advent-of-code-2018.day1
  (:require [clojure.string :as s]))

(def input
  (map #(Integer/parseInt %)
       (s/split-lines (slurp "inputs\\day1.txt"))))

(def part-1 (partial reduce +))

(defn part-2 [input]
  (reduce (fn [cache val]
            (if (contains? cache val)
              (reduced val)
              (conj cache val)))
          #{}
          (reductions + 0 (cycle input))))
