(ns advent-of-code-2018.day3
  (:require [clojure.string :as s]))

(def input (map (comp (partial map #(Integer/parseInt %))
                      rest
                      (partial re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"))
                (s/split-lines (slurp "inputs\\day3.txt"))))

(defn coords [[_ x y width height]]
  (for [w (range width)
        h (range height)]
    [(+ x w) (+ y h)]))

(defn fill [fabric claim]
  (reduce (fn [f coords]
            (if (contains? f coords)
              (update f coords inc)
              (assoc f coords 1)))
          fabric
          (coords claim)))

(defn part-1 [input]
  (count
   (filter (comp (partial < 1) val)
           (reduce fill {} input))))

(defn part-2 [input]
  (let [not-overlapping (set
                         (map first
                              (filter (comp (partial = 1) val)
                                      (reduce fill {} input))))]
    (ffirst
     (filter (fn [claim]
               (every? (partial contains? not-overlapping) (coords claim)))
             input))))
