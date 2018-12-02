(ns advent-of-code-2018.util)

(defmacro either [& vals]
  (let [sym (gensym)]
    `(fn [~sym] (or ~@(map (fn [x] `(= ~sym ~x)) vals)))))
