(ns advent-of-code-2018.day12
  (:require [advent-of-code-2018.util :as u]
            [clojure.string :as s]))

(defn transform [rule]
  (map (fn [val] (if (= val \#) 1 0))
       (filter (u/either \# \.) rule)))

(defn start [[initial & rules]]
  {:state (vec (map vector (range) initial))
   :rules (map (fn [rule]
                 {:state (take 5 rule) :result (last rule)})
               rules)})

(def input (->> (slurp "inputs\\day12.txt")
                s/split-lines
                (map transform)
                (remove empty?)
                start))

(defn ensure-size [values]
  (let [lidx (ffirst values)
        ridx (first (last values))
        offset-left (count (take-while (comp (partial = 0) second) values))
        offset-right (count (take-while (comp (partial = 0) second) (rseq values)))
        left (map vector (range (- lidx (- 4 offset-left)) lidx) (repeat 0))
        right (map vector (range (inc ridx) (- (+ 5 ridx) offset-right)) (repeat 0))]
    (concat left values right)))

(def resolve-rule
  (memoize
   (fn [rules state]
     (->> rules
          (filter (comp (partial = state) :state))
          first
          :result))))

(def life
  (memoize
   (fn [rules state]
     [(ffirst (drop 2 state)) (resolve-rule rules (map second state))])))

(defn life-iter [{:keys [state rules] :as plants}]
  (lazy-seq
   (let [next-state (->> state
                         ensure-size
                         (partition 5 1)
                         (map (partial life rules))
                         vec)]
     (cons next-state (life-iter (assoc plants :state next-state))))))

(defn sum-alive [generations input]
  (->> input
       life-iter
       (drop (dec generations))
       first
       (filter (comp (partial = 1) second))
       (map first)
       (reduce +)))

(def part-1 (partial sum-alive 20))

(defn part-2 [input]
  (let [gradients (map (juxt identity #(sum-alive % input)) (range 500 2001 500))
        diff (fn [selection] (->> gradients
                                  (map selection)
                                  reverse
                                  (reduce -)))
        [x y] [(diff first) (diff second)]]
    (* 50000000000 (/ y x))))
