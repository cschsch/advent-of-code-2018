(ns advent-of-code-2018.day2
  (:require [clojure.string :as s]
            [advent-of-code-2018.util :as u]))

(def input (s/split-lines (slurp "inputs\\day2.txt")))

(def freq-vals (comp vals frequencies))

(defn part-1 [input]
  (->> input
       (mapcat (comp distinct
                     (partial filter (u/either 2 3))
                     freq-vals))
       freq-vals
       (reduce *)))

(def eq-or-nil #(when (= %1 %2) %1))

(defn one-off? [x y]
  (->> [x y]
       (apply map eq-or-nil)
       (filter nil?)
       count
       (= 1)))

(defn part-2 [input]
  (let [[match] (for [x input
                      y input
                      :when (one-off? x y)]
                  [x y])]
    (->> match
         (apply map eq-or-nil)
         (filter identity)
         (apply str))))
