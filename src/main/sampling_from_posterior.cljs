(ns sampling-from-posterior
  (:require [oz.core :as oz]
            [graphs :as g]
            [dbinomial :as d]
            [semantic-ui-react :as sur]))


;; (defonce app-state (r/atom {:play-timeout-ID nil
;;                             :speed 1000}))



(defn graph-posterior-dis [grid-p samples]
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
         (g/data grid-p
                 (->>
                  (d/r-likelihood-from-samples grid-p samples)
                  d/standardize))
         (g/titles  "Posterior Pr(p ⎹ W,L)"
                    "% of world that is water"
                    "Pr(p ⎹ W,L)"))]
    {:hconcat [n-graph pos-graph]}))

;; (defn play []
;;   (swap! app-state new-random-sample)
;;   (swap! app-state assoc-in [:play-timeout-ID]
;;          (if (not-reached-sample-limit (:samples @app-state))
;;            (js/setTimeout play (:speed @app-state))
;;            nil)))


;; (defn pause []
;;   (js/clearTimeout (:play-timeout-ID @app-state))
;;   (swap! app-state assoc-in [:play-timeout-ID] nil))


;; (defn buttons []
;;   (let [tool-tip "Randomly generated samples are :w with a probability of 0.6."]
;;     [:div#buttons
;;      [:> sur/Popup {:content tool-tip
;;                     :trigger (r/as-element
;;                               (if (:play-timeout-ID @app-state)
;;                                 [:> sur/Button
;;                                  {:onClick (fn []
;;                                              (js/console.log "Pause pressed")
;;                                              (pause))}
;;                                  [:> sur/Icon {:name "pause"}]]
;;                                 [:> sur/Button
;;                                  {:onClick (fn []
;;                                              (js/console.log "Play pressed")
;;                                              (if (not-reached-sample-limit (:samples @app-state))
;;                                                nil
;;                                                (swap! app-state assoc-in [:samples] []))
;;                                              (play))}
;;                                  [:> sur/Icon {:name "play"}]]))}]
;;      "Speed : "
;;      [:input {:type "range" :value (:speed @app-state) :min 125 :max 2000 :step 125
;;               :tooltip (str " - new sample every " (/ (:speed @app-state) 1000) " seconds")
;;               :on-change (fn [e]
;;                            (let [new-value (js/parseInt (.. e -target -value))]
;;                              (swap! app-state assoc-in [:speed] new-value)))}]
;;      (str (.toFixed (/ 1000 (:speed @app-state)) 2) " samples/second")
;;      (when (not-reached-sample-limit (:samples @app-state))
;;        [:> sur/Popup {:content tool-tip
;;                       :trigger (r/as-element
;;                                 [:> sur/Button
;;                                  {:onClick (fn [] (swap! app-state new-random-sample))}
;;                                  "Random sample"])}])
;;      (when (not-reached-sample-within-10-of-limit (:samples @app-state))
;;        [:> sur/Popup {:content tool-tip
;;                       :trigger (r/as-element
;;                                 [:> sur/Button
;;                                  {:onClick (fn [] (swap! app-state ten-new-random-samples))}
;;                                  "x 10"])}])
;;      (when (not-reached-sample-limit (:samples @app-state))
;;        [:> sur/Button
;;         {:onClick (fn [] (swap! app-state user-sample :w))}
;;         ":w"])
;;      (when (not-reached-sample-limit (:samples @app-state))
;;        [:> sur/Button
;;         {:onClick (fn [] (swap! app-state user-sample :l))}
;;         ":l"])
;;      (when (last (:samples @app-state))
;;        [:> sur/Button
;;         {:onClick (fn []
;;                     (js/console.log (str "Remove 1 sample " (:samples @app-state)))
;;                     (swap! app-state one-less-sample))}
;;         "Remove a sample"])
;;      (when (>= (count (:samples @app-state)) 10)
;;        [:> sur/Button
;;         {:onClick (fn [] (swap! app-state ten-less-samples))}
;;         "x 10"])
;;      (when (last (:samples @app-state))
;;        [:> sur/Button
;;         {:onClick (fn []
;;                     (js/console.log (str "Clear samples " (:samples @app-state)))
;;                     (swap! app-state assoc-in [:samples] []))}
;;         "Clear samples"])]))


(defn page []
  (let [grid-p (map #(/ % 200) (range 0 201))
        samples (repeatedly 100 #(if (>= 0.6 (rand)) :w :l))]
    [:> sur/Container
     [oz/vega-lite (graph-posterior-dis grid-p samples)]]))
