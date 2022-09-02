(ns core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [oz.core :as oz]
            [graphs :as g]
            [dbinomial :as d]))

(defonce random-samples (into []  (take 20 (repeatedly (fn [] (if (>= 0.6 (rand)) :w :l))))))
(defonce samples (r/atom '()))

(def coll-p (map #(/ % 200) (range 0 201)))

(defn graph-posterior-dis [samples]
  {:hconcat
   [(apply g/land-or-water (d/count-land-or-water samples))
    (g/probability-dis "Relative Likelihood" "Pr(W,L|p)" coll-p (d/r-likelihood-from-samples coll-p samples))
    (g/probability-dis "Posterior Probability (standardized)" "Pr(p|W,L)" coll-p
                       (->
                        (d/r-likelihood-from-samples coll-p samples)
                        d/standardize))]})

(defn one-more-sample [samples]
  (take (inc (count samples)) random-samples))

(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [:div
                [oz/vega-lite (graph-posterior-dis @samples)]
                [:div [:button
                 {:onClick (fn [] (swap! samples one-more-sample))}
                 "Next sample"]
                [:div @samples]]]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
