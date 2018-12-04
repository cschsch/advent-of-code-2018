(ns advent-of-code-2018.day4
  (:require [clojure.string :as s]
            [clj-time.core :as t]
            [clj-time.periodic :as tp]
            [clj-time.format :as tf]))

(def date-formatter (tf/formatter "yyyy-MM-dd HH:mm"))

(defn organize-list [[date id]]
  (let [time {:time (tf/parse date-formatter date)}]
    (if id
      (assoc time :id (Integer/parseInt id))
      time)))

(defn connect [[x & rest :as coll]]
  (let [[xs rest] (split-with (comp not :id) rest)]
    (when (seq coll)
      (concat [(cons x xs)] (connect2 rest)))))

(def input
  (->> (slurp "inputs\\day4.txt")
       s/split-lines
       (map (comp organize-list
                  next
                  (partial re-find #"\[(.+)\] \w+ (?:\w+|#(\d+))")))
       (sort-by :time)
       connect))

(defn sleep-intervals [[{sleep :time} {wake :time} & rest :as coll]]
  (when (seq coll)
    (cons (t/interval sleep wake) (sleep-intervals rest))))

(defn most-asleep [shifts]
  (first
   (sort-by (comp -
                  (partial reduce
                           #(+ %1 (t/in-minutes %2))
                           0)
                  val)
            shifts)))

(defn most-overlaps [[id intervals]]
  (cons id (->> intervals
                (mapcat (comp (partial map t/minute)
                              #(tp/periodic-seq (.getStart %) (.getEnd %) (org.joda.time.Period. (* 1000 60)))))
                frequencies
                (sort-by second)
                last)))

(defn part-1 [input]
  (->> input
       (reduce (fn [m [{id :id} & sleep]]
                   (update m id concat (sleep-intervals sleep)))
                 {})
       most-asleep
       most-overlaps
       (take 2)
       (reduce *)))

(def third (comp first (partial drop 2)))

(defn part-2 [input]
  (->> input
       (reduce (fn [m [{id :id} & sleep]]
                    (update m id concat (sleep-intervals sleep)))
               {})
       (map most-overlaps)
       (sort-by third)
       last
       (take 2)
       (reduce *)))
