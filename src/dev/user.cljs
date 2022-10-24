
(comment
  (let [samples (repeatedly 100 #(if (>= 0.6 (rand)) :w :l))
        pos-dis (dbinomial/posterior-distribution samples)
        max-pos-dis (apply max pos-dis)]
    max-pos-dis)
  (->> (repeatedly 100 #(if (>= 0.6 (rand)) :w :l))
       dbinomial/posterior-distribution
       (apply max))
  (->> (repeatedly 100 #(if (>= 0.6 (rand)) :w :l))
       dbinomial/count-land-or-water)
  (def samples (repeatedly 100 #(if (>= 0.6 (rand)) :w :l)))
  (->> (repeatedly 100 #(if (>= 0.6 (rand)) :w :l))
       dbinomial/sample-posterior)
  )