(ns sampling-from-posterior
  (:require [reagent.core :as r]
            [oz.core :as oz]
            [graphs :as g]
            [dbinomial :as d]
            [semantic-ui-react :as sur]
            [react-custom :as rc]))

(defonce samples (repeatedly 100 #(if (>= 0.6 (rand)) :w :l)))

(defonce app-state (r/atom {:pos-dis-samples []
                            :play-timeout-ID nil
                            :speed 50.0}))



(defn graph-posterior-dis []
  (let [[n land water] (d/count-land-or-water samples)
        n-graph
        (g/bar-chart
         (g/titles (str "n = " n)  "Land (L) or Water (W)?" "Percent of Sample")
         {:data {:values
                 [{:x "W" :y (if (zero? n) 0 (/ water n))}
                  {:x "L" :y (if (zero? n) 0 (/ land n))}]}}
         (g/percentage-axis :y))
        pos-graph
        (g/probability-dis
         (g/data d/grid-p (d/posterior-distribution samples))
         (g/titles  "Posterior Pr(p ⎹ W,L)"
                    "% of world that is water"
                    "Pr(p ⎹ W,L)"))]
    {:hconcat [n-graph pos-graph]}))

(def max-samples 1e4)

(defn not-reached-sample-limit [samples]
  (< (count samples) max-samples))

(defn new-random-sample [state samples]
  (update-in state [:pos-dis-samples] conj (d/sample-posterior samples)))

(defn play []
  (swap! app-state new-random-sample samples)
  (swap! app-state assoc-in [:play-timeout-ID]
         (if (not-reached-sample-limit (:pos-dis-samples @app-state))
           (js/setTimeout play (js/Math.floor (/ 1000 (:speed @app-state))))
           nil)))

(defn pause []
  (js/clearTimeout (:play-timeout-ID @app-state))
  (swap! app-state assoc-in [:play-timeout-ID] nil))

(defn buttons []
  (let [tool-tip "Sampling from under the curve of probability density function."]
    [:div#buttons
     [:> sur/Popup {:content tool-tip
                    :trigger (r/as-element
                              (if (:play-timeout-ID @app-state)
                                [:> sur/Button
                                 {:onClick pause}
                                 [:> sur/Icon {:name "pause"}]]
                                [:> sur/Button
                                 {:onClick (fn []
                                             (when (not (not-reached-sample-limit (:pos-dis-samples @app-state)))
                                               (swap! app-state assoc-in [:pos-dis-samples] []))
                                             (play))}
                                 [:> sur/Icon {:name "play"}]]))}]
     "Speed : "
     [:input {:type "range" :value (:speed @app-state) :min 20 :max 100 :step 10
              :on-change (fn [e]
                           (let [new-value (js/parseFloat (.. e -target -value))]
                             (swap! app-state assoc-in [:speed] new-value)))}]
     (str (:speed @app-state) " samples/second") 
     (when (last (:pos-dis-samples @app-state))
       [:> sur/Button
        {:onClick (fn []
                    (swap! app-state assoc-in [:pos-dis-samples] []))}
        "Clear samples"])]))

(defn page []
  [:> sur/Container
   [oz/vega-lite (graph-posterior-dis)]
   [buttons samples]
   [rc/collapsible
    "Samples"
    [:div (str (:pos-dis-samples @app-state))]]
   [oz/vega-lite 
    {:hconcat
     [(g/point-chart
      (g/data (range 0 1e4) (:pos-dis-samples @app-state))
      (g/titles  "Random Samples from Posterior"
                 "sample number"
                 "% of world that is water"))
     (g/area-chart
      (g/data (:pos-dis-samples @app-state))
      (g/density-transform 0.01 [0 1])
      (g/titles  "Density of Samples from Posterior"
                 "% of world that is water"
                 "Density"))]
     }]])
