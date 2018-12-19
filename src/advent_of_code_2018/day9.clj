(ns advent-of-code-2018.day9
  (:require [advent-of-code-2018.dll :as dll]))

(def input (map #(Integer/parseInt %)
                (next (re-find #"(\d+).+?(\d+)"
                               (slurp "inputs\\day9.txt")))))

(defn process-points [{[marbles pos :as loc] :board [p] :current-player :as game}]
  (let [[_ to-remove :as moved] (dll/nth loc dll/left 7)
        next-val (inc pos)
        new-marbles (-> moved
                        dll/right
                        (dll/rmv-loc to-remove)
                        dll/right
                        (dll/conjr (inc next-val))
                        dll/right)]
    (-> game
        (assoc :board new-marbles)
        (update-in [:scores p] (partial + next-val to-remove))
        (update :current-player (partial drop 2)))))

(defn next-marble [[_ pos :as marbles]]
  (-> marbles
      dll/right
      (dll/conjr (inc pos))
      dll/right))

(defn next-iteration [game]
  (-> game
      (update :board next-marble)
      (update :current-player rest)))

(defn play [[players last-marble]]
  (->> (iterate
        (fn [{[_ marble] :board :as game}]
          (when (= 0 (mod marble 1000000)) (println marble))
          (if (= 0 (mod (inc marble) 23))
            (process-points game)
            (next-iteration game)))
        {:board (dll/doubly-linked-list-loc 0)
         :current-player (cycle (range players))
         :scores (vec (repeat players 0))})
       (filter (comp (partial = last-marble) dll/pos :board))
       first))

(defn part-1 [input]
  (->> input
       play
       :scores
       (apply max)))

(defn part-2 [[players last-marble]]
  (part-1 [players (* last-marble 100)]))
