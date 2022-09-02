(ns core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [oz.core :as oz]
            [graphs :as g]
            [dbinomial :as d]))

(defonce random-samples (into []  (take 50 (repeatedly (fn [] (if (>= 0.6 (rand)) :w :l))))))
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

(defn more-samples-available [samples]
  (< (count samples) (count random-samples)))

(defn one-more-sample [samples]
  (if (more-samples-available samples) 
    (take (inc (count samples)) random-samples)
    '()))

(defn play []
  (js/console.log "Play pressed")
  (swap! samples one-more-sample)

  (if (more-samples-available @samples)
    (js/setTimeout play 500)
    nil))

(defn page []
  [:div
   [oz/vega-lite (graph-posterior-dis @samples)]
   [:div
    [:button#play
     {:onClick play}
     "▶️"]
    [:button
     {:onClick (fn []
                 (js/console.log (str "Button pressed - samples " @samples))
                 (swap! samples one-more-sample))}
     (if (more-samples-available @samples) "Next sample" "Clear samples")]
    (str "  " @samples)]])

(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [page]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
