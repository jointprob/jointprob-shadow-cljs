(ns sampling-from-posterior
  (:require [reagent.core :as r]
            [oz.core :as oz]
            [graphs :as g]
            [dbinomial :as d]
            [semantic-ui-react :as sur]))

(defonce samples (repeatedly 100 #(if (>= 0.6 (rand)) :w :l)))

(defonce ten-thousand-pos-dis-samples (repeatedly 1e4 #(d/sample-posterior samples)))

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

(def max-samples 2e3)

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
     [:input {:type "range" :value (:speed @app-state) :min 5 :max 50 :step 5
              :on-change (fn [e]
                           (let [new-value (js/parseFloat (.. e -target -value))]
                             (swap! app-state assoc-in [:speed] new-value)))}]
     (str (:speed @app-state) " samples/second") 
     (when (last (:pos-dis-samples @app-state))
       [:> sur/Button
        {:onClick (fn []
                    (swap! app-state assoc-in [:pos-dis-samples] []))}
        "Clear samples"])]))

(defn pos-dis-samples-graph [pos-dis-samples]
  (let [binned (frequencies (map d/round-number-to-grid pos-dis-samples))
        zeroes (zipmap d/grid-p (repeat 0))
        zeroes-added (apply conj zeroes binned)]
    [oz/vega-lite
     {:hconcat
      [(g/point-chart
        (g/data (range 0 (count pos-dis-samples)) pos-dis-samples)
        (g/titles  (str (count pos-dis-samples)
                        " Samples from Posterior")
                   "sample number"
                   "% of world that is water"))
       (g/point-chart
        (g/mark-properties {:size 10 :opacity 1})
        (g/data (keys zeroes-added) (vals zeroes-added))
        (g/titles  "Count of Samples in 200 Bins"
                   "% of world that is water"
                   "Count"))
       (g/point-chart
        (g/mark-properties {:size 10 :opacity 1})
        (g/data (keys zeroes-added) (d/standardize (vals zeroes-added)))
        (g/titles  "Standardize Counts to Average 1"
                   "% of world that is water"
                   "Density"))]}]))

(defn page []
  [:> sur/Container
   [oz/vega-lite (graph-posterior-dis)]
   [buttons]
   [pos-dis-samples-graph (:pos-dis-samples @app-state)]
   [pos-dis-samples-graph ten-thousand-pos-dis-samples]
   ])

(comment
  (g/point-chart
   (g/size-of-mark 25)
   (g/data [0] [1])
   (g/titles  "Count of Samples in 200 Bins"
              "% of world that is water"
              "Count"))
  )