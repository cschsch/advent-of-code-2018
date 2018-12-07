(ns advent-of-code-2018.day7
  (:require [clojure.string :as s]
            [clojure.set :as se]))

(def input (set (map (comp vec
                           rest
                           (partial re-find #"Step (\w).+ step (\w)"))
                     (s/split-lines (slurp "inputs\\day7.txt")))))

(defn start [input]
  (let [requirements (set (map first input))
        dependents (set (map second input))
        independent (se/difference requirements dependents)]
    (apply sorted-set (filter (partial some independent) input))))

(defn dependencies [input element]
  (apply sorted-set (filter (comp (partial = element) first) input)))

(defn remove-dependencies [input element]
  (apply disj input (dependencies input element)))

(defn part-1 [input]
  (lazy-seq
   (when (seq input)
     (let [next-element (ffirst (start input))]
       (if (= 1 (count input))
         (list next-element (second (first (start input))))
         (cons next-element (part-1 (remove-dependencies input next-element))))))))

#_todo
(defn part-2 [input]
  nil)
