(ns advent-of-code-2018.day5
  (:require [clojure.set :as s]
            [advent-of-code-2018.util :as u]))

(def input (slurp "inputs\\day5.txt"))

(defn same-type? [a b]
  (letfn [(add-char [x c] (char (+ x (int c))))]
    (or (= a b) (= (add-char 32 a) b) (= a (add-char 32 b)))))

(defn lowercase? [c]
  (<= 97 (int c) 122))

(defn uppercase? [c]
  (<= 65 (int c) 90))

(defn both? [pred & args]
  (every? pred args))

(defn same-polarity? [a b]
  (or (both? lowercase? a b)
      (both? uppercase? a b)))

(def react?
  (memoize
   (fn [a b]
     (and (same-type? a b)
          (not (same-polarity? a b))))))

(defn contains-pair? [s [a b :as pair]]
  (let [without-itself (s/difference s (hash-set pair))]
    (some (fn [[x y]] (or (= a x) (= a y))) without-itself)))

(defn part-1 [input]
  (let [indices (->> input
                     (map-indexed vector)
                     (partition 2 1)
                     (filter (comp (partial apply react?)
                                   (partial map second)))
                     flatten
                     (take-nth 2)
                     (partition 2)
                     set)
        to-remove (set (flatten (remove (partial contains-pair? indices) indices)))]
    (if (empty? to-remove)
      (count input)
      #(part-1 (filter identity (map-indexed (fn [i x] (when-not (contains? to-remove i) x)) input))))))

(defn pairs [pred s]
  (lazy-seq
   (let [x (hash-set (first s))
         match (s/union (hash-set (first (filter (partial pred (first x)) (s/difference s x)))) x)]
     (when (not-empty s)
       (cons match (pairs pred (s/difference s match)))))))

(defn part-2 [input]
  (let [types (pairs same-type? (set input))
        without-types (map #(remove % input) types)]
    (apply min (pmap (partial trampoline part-1) without-types))))

