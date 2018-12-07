(ns advent-of-code-2018.day6
  (:require [clojure.string :as s]))

(def input (map (comp (partial map #(Integer/parseInt %))
                      #(s/split % #", "))
                (s/split-lines (slurp "inputs\\day6.txt"))))

(defn grid [ps]
  (for [x (range (apply min (map first ps)) (inc (apply max (map first ps))))
        y (range (apply min (map second ps)) (inc (apply max (map second ps))))]
    [x y]))

(defn manhattan [p1 p2]
  (reduce + (map (comp #(Math/abs %) -) p1 p2)))

(defn closest [ps p]
  (let [manhattans (group-by (partial manhattan p) ps)
        smallest (apply min (map key manhattans))]
    (get manhattans smallest)))

(defn extending? [bounds p]
  (some (hash-set p) bounds))

(defn part-1 [input]
  (let [[min-x max-x] [(apply min (map first input)) (apply max (map first input))]
        [min-y max-y] [(apply min (map second input)) (apply max (map second input))]
        bounds (set (concat (map (partial vector min-x) (range min-y (inc max-y)))
                            (map (partial vector max-x) (range min-y (inc max-y)))
                            (map #(vector % min-y) (range min-x (inc max-x)))
                            (map #(vector % max-y) (range min-x (inc max-x)))))
        ps-and-closest (filter (comp empty? rest second)
                               (pmap (juxt identity (partial closest input)) (grid input)))
        cache (reduce (fn [c [p closest]]
                        (if (extending? bounds p)
                          (conj c closest)
                          c))
                      #{}
                      ps-and-closest)
        areas-and-ps (->> ps-and-closest
                          (pmap second)
                          frequencies)]
    (apply max (reduce (fn [as [p freq]]
                         (if (contains? cache p)
                           as
                           (conj as freq)))
                       []
                       areas-and-ps))))

(defn part-2 [input]
  (->> input
       grid
       (filter (comp (partial > 10000)
                     (partial reduce +)
                     #(pmap (partial manhattan %) input)))
       count))
