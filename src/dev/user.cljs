(ns user
  (:require [sampling-from-posterior :as s]
            [dbinomial :as d]))

(comment
  (let [samples (repeatedly 100 #(if (>= 0.6 (rand)) :w :l))
        pos-dis (d/posterior-distribution samples)
        max-pos-dis (apply max pos-dis)]
    max-pos-dis)
  (->> (repeatedly 100 #(if (>= 0.6 (rand)) :w :l))
       d/posterior-distribution
       (apply max))
  (->> (repeatedly 100 #(if (>= 0.6 (rand)) :w :l))
       d/count-land-or-water)
  (def samples (repeatedly 100 #(if (>= 0.6 (rand)) :w :l)))
  (->> (repeatedly 100 #(if (>= 0.6 (rand)) :w :l))
       d/sample-posterior)
  )