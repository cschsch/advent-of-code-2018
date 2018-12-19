(ns advent-of-code-2018.dll
  (:require [clojure.test :refer [is with-test]]))

(with-test
  (defn doubly-linked-list [x]
    {x [x x]})
  (is (= {0 [0 0]} (doubly-linked-list 0)))
  (is (= {"haha" ["haha" "haha"]} (doubly-linked-list "haha")))
  (is (= {nil [nil nil]} (doubly-linked-list nil))))

(def doubly-linked-list-loc
  (juxt doubly-linked-list identity))

(def count-loc (comp count first))

(def value first)
(def pos second)

(with-test
  (defn right
    ([l pos] (second (l pos)))
    ([[l pos]] [l (right l pos)]))
  (is (let [val 9001
            l (doubly-linked-list val)
            rght (partial right l)]
        (= val (rght val) (rght (rght (rght (rght val))))))))

(with-test
  (defn left
    ([l pos] (first (l pos)))
    ([[l pos]] [l (left l pos)]))
  (is (let [val 9001
            l (doubly-linked-list val)
            lft (partial left l)]
        (= val (lft val) (lft (lft (lft (lft val))))))))

(with-test
  (defn conjl
    ([l pos x]
     (when (l pos)
       (let [prev (left l pos)]
         (-> l
             (assoc x [prev pos])
             (assoc-in [pos 0] x)
             (assoc-in [prev 1] x)))))
    ([[l pos] x]
     (when (l pos)
       [(conjl l pos x) pos])))
  (is (= {"dos" ["uno" "uno"] "uno" ["dos" "dos"]}
         (value (conjl (doubly-linked-list-loc "uno") "dos"))))
  (is (= {1 [0 2] 0 [2 1] 2 [1 0]}
         (-> (doubly-linked-list-loc 1)
             (conjl 2)
             (conjl 0)
             value))))

(with-test
  (defn conjr
    ([l pos x]
     (when (l pos)
       (let [ahead (right l pos)]
         (-> l
             (assoc x [pos ahead])
             (assoc-in [pos 1] x)
             (assoc-in [ahead 0] x)))))
    ([[l pos] x]
     (when (l pos)
       [(conjr l pos x) pos])))
  (is (= (value (conjl (doubly-linked-list-loc "uno") "dos"))
         (value (conjr (doubly-linked-list-loc "uno") "dos"))))
  (is (= {1 [0 2] 0 [2 1] 2 [1 0]}
         (-> (doubly-linked-list-loc 1)
             (conjr 2)
             left
             (conjr 0)
             value))))

(defn- values
  ([next-val l pos]
   (lazy-seq (cons pos (values next-val l (next-val l pos)))))
  ([next-val [l pos]]
   (values next-val l pos)))

(def valuesr (partial values right))
(def valuesl (partial values left))

(defn nth
  ([l pos dir n]
   (clojure.core/nth (iterate (partial dir l) pos) n))
  ([[l pos] dir n]
   [l (nth l pos dir n)]))

(with-test
  (defn rmv [l x]
    (when (l x)
      (let [[prev ahead] (l x)]
        (-> l
            (dissoc x)
            (assoc-in [prev 1] ahead)
            (assoc-in [ahead 0] prev)))))
  (is (= {1 [0 2] 0 [2 1] 2 [1 0]}
         (-> (doubly-linked-list-loc 1)
             (conjr 2)
             (conjl 0)
             (conjl 0.5)
             value
             (rmv 0.5)))))

(defn rmv-loc [[l pos] x]
  (when (l pos)
    [(rmv l x) pos]))
