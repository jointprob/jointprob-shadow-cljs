(ns graphs)

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))


(defn bar-chart [ & vl-maps]
  (apply deep-merge {:mark     {:type :bar},
                     :encoding {:x {:field :x, :type "nominal"},
                                :y {:field :y, :type "quantitative"}}}
              vl-maps))

(defn titles [graph-title x-title y-title]
  {:title graph-title
   :encoding {:x {:title x-title}
              :y {:title y-title}}})

(defn data
  ([xs]
   {:data {:values (map #(hash-map :x %1) xs)}})
  ([xs ys]
   {:data {:values (map #(hash-map :x %1 :y %2) xs ys)}}))

(defn density-transform [bandwidth extent]
  {:transform [{
                :density :x
                :count true
                :bandwidth bandwidth
                :extent extent
                }]
   :encoding {:x {:field :value
                  :type "quantitative"}
              :y {:field :density
                  :type "quantitative"}}})
  

(defn axis-format [axis format]
  {:encoding {axis {:axis {:format format}}}})

(defn percentage-axis [axis]
  (axis-format axis "p"))

(defn continuous-x-y-chart[ & vl-maps]
  (apply deep-merge
         {:encoding {:x {:field :x :type "quantitative"}
                     :y {:field :y :type "quantitative"}}}
         vl-maps))

(defn line-chart [ & vl-maps]
  (apply continuous-x-y-chart (conj vl-maps {:mark {:type :line}})))

(defn area-chart [& vl-maps]
  (apply continuous-x-y-chart (conj vl-maps {:mark {:type :area}})))

(defn probability-dis [ & vl-maps]
  (deep-merge
         (apply line-chart vl-maps)
         (percentage-axis :x)))

(defn size-of-mark [size]
  {:mark {:size size}})
         

(defn point-chart [ & vl-maps]
  (apply continuous-x-y-chart (conj vl-maps {:mark {:type :point
                                                    :opacity 0.5
                                                    :filled true
                                                    :size 5}})))
