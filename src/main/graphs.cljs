(ns graphs)

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))


(defn bar-chart [ & vl-maps]
  (apply deep-merge {:mark     "bar",
                     :encoding {:x {:field :x, :type "nominal"},
                                :y {:field :y, :type "quantitative"}}}
              vl-maps))

(defn titles [graph-title x-title y-title]
  {:title graph-title
   :encoding {:x {:title x-title}
              :y {:title y-title}}})

(defn data
  [xs ys]
  {:data {:values (map #(hash-map :x %1 :y %2) xs ys)}})

(defn percentage-axis [axis]
  {:encoding {axis {:axis {:format :p}}}})

(defn continuous-x-y-chart[ & vl-maps]
  (apply deep-merge
         {:encoding {:x {:field :x :type "quantitative"}
                     :y {:field :y :type "quantitative"}}}
         vl-maps))

(defn line-chart [ & vl-maps]
  (apply continuous-x-y-chart (conj vl-maps {:mark :line})))

(defn probability-dis [ & vl-maps]
  (deep-merge
         (apply line-chart vl-maps)
         (percentage-axis :x)))
         

(defn point-chart [ & vl-maps]
  (apply continuous-x-y-chart (conj vl-maps {:mark {:type :point :opacity 0.5}})))
