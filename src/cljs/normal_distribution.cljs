(ns normal-distribution
  (:require [dbinomial :as d]
            [graphs :as g]
            [react-vega]
            [reagent.core :as r]
            [semantic-ui-react :as sur]
            [reagent-custom :as rc]
            [kixi.stats.math :as km]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]))


(defonce page-state (r/atom {:no-of-height-samples 0
                            :play-timeout-ID nil
                            :speed 1.0
                            :collapsed
                            {:question false
                             :prior false}}))

(def adult-heights [151.765 139.7 136.525 156.845 145.415 163.83 149.225 168.91 147.955 
                    165.1 154.305 151.13 144.78 149.9 150.495 163.195 157.48 143.9418 
                    161.29 156.21 146.4 148.59 147.32 147.955 161.925 146.05 146.05 
                    152.7048 142.875 142.875 147.955 160.655 151.765 162.8648 171.45 
                    147.32 147.955 154.305 143.51 146.7 157.48 165.735 152.4 141.605 
                    158.8 155.575 164.465 151.765 161.29 154.305 145.415 145.415 152.4 
                    163.83 144.145 153.67 142.875 167.005 158.4198 165.735 149.86 154.94 
                    160.9598 161.925 147.955 159.385 148.59 136.525 158.115 144.78 
                    156.845 179.07 170.18 146.05 147.32 162.56 152.4 160.02 149.86 
                    142.875 167.005 159.385 154.94 162.56 152.4 170.18 146.05 159.385 
                    151.13 160.655 169.545 158.75 149.86 153.035 161.925 162.56 149.225 
                    163.195 161.925 145.415 163.195 151.13 150.495 170.815 157.48 152.4 
                    147.32 145.415 157.48 154.305 167.005 142.875 152.4 160 159.385 149.86 
                    160.655 160.655 149.225 140.97 154.94 141.605 160.02 150.1648 155.575 
                    156.21 153.035 167.005 149.86 147.955 159.385 161.925 155.575 159.385 
                    146.685 172.72 166.37 141.605 151.765 156.845 148.59 157.48 149.86 
                    147.955 153.035 160.655 149.225 138.43 162.56 149.225 158.75 149.86 
                    158.115 156.21 148.59 143.51 154.305 157.48 157.48 154.305 168.275 
                    145.415 149.225 154.94 162.56 156.845 161.0106 144.78 143.51 149.225 
                    149.86 165.735 144.145 157.48 154.305 163.83 156.21 144.145 162.56 
                    146.05 154.94 144.78 146.685 152.4 163.83 165.735 156.21 152.4 
                    140.335 163.195 151.13 171.1198 149.86 163.83 141.605 149.225 146.05 
                    161.29 162.56 145.415 170.815 159.385 159.4 153.67 160.02 150.495 
                    149.225 142.875 142.113 147.32 162.56 164.465 160.02 153.67 167.005 
                    151.13 153.035 139.065 152.4 154.94 147.955 144.145 155.575 150.495 
                    155.575 154.305 157.48 168.91 150.495 160.02 167.64 144.145 145.415 
                    160.02 164.465 153.035 149.225 160.02 149.225 153.67 150.495 151.765 
                    158.115 149.225 151.765 154.94 161.29 148.59 160.655 157.48 167.005 
                    157.48 152.4 152.4 161.925 152.4 159.385 142.24 168.91 160.02 158.115 
                    152.4 155.575 154.305 156.845 156.21 168.275 147.955 157.48 160.7 
                    161.29 150.495 163.195 148.59 148.59 161.925 153.67 151.13 163.83 
                    153.035 151.765 156.21 140.335 158.75 142.875 151.9428 161.29 160.9852 
                    144.78 160.02 160.9852 165.989 157.988 154.94 160.655 147.32 146.7 
                    147.32 172.9994 158.115 147.32 165.989 149.86 161.925 163.83 160.02 
                    154.94 152.4 146.05 151.9936 151.765 144.78 160.655 151.13 153.67 
                    147.32 139.7 157.48 154.94 143.51 158.115 147.32 160.02 165.1 154.94 
                    153.67 141.605 163.83 161.29 154.9 161.3 170.18 149.86 160.655 154.94 
                    166.37 148.2852 151.765 148.59 153.67 146.685 154.94 156.21 160.655 
                    146.05 156.21 152.4 162.56 142.875 162.56 156.21 158.75])


(defn inclusive-range [start end steps]
  (let [step (/ (- end start) steps)]
    (range start (+ end step) step)))

(def grid (for [mu (inclusive-range 150 160 40)
                sd (inclusive-range 7 9 40)]
            {:mu mu :sd sd}))

(defn dnorm [x mu sd]
  (/
   (km/exp
    (/ (* -0.5 (km/sq (- x mu))) (km/sq sd)))
   (* (km/sqrt (* 2 km/PI)) sd)))

(defn relative-likelihood-for-grid [grid x]
  (map (fn [{:keys [sd mu]} x] (dnorm x mu sd)) grid (repeat x)))

(def relative-likelihood-for-grid-for-all-h
  (map #(->> %
             (relative-likelihood-for-grid grid)
             d/standardize)
       adult-heights))


(defn heat-map-graph
  ([title data z-title]
   (hc/xform ht/heatmap-chart
             :TITLE title
             :WIDTH 300
             :HEIGHT 300
             :DATA data
             :X "mu"
             :XTYPE "nominal"
             :Y "sd"
             :YTYPE "nominal"
             :YAXIS {:format "3~f"}
             :COLOR {:field "ll" :type "quantitative" :title z-title}
             :YSCALE {:zero false}
             :XSCALE {:zero false}
             :TOOLTIP [{:field "ll" :title z-title :type "quantitative"}
                       {:field "mu" :type "quantitative"}
                       {:field "sd" :type "quantitative"}]))
  ([title data]
   (heat-map-graph title data "Relative likelihood")))

;; We'll ignore the uniform prior for sd as it will have no effect on the posterior probability after
;; standardisation. Our prior for mu is a normal distribution with mean 178 and standard deviation 20.

(def prior (map (fn [{:keys [mu _]}] (dnorm mu 178.0 20)) grid))

(def products-of-likelihoods
  (reductions #(->
                (map * %1 %2)
                d/standardize)
              relative-likelihood-for-grid-for-all-h))

(def posteriors
  (reductions #(->
                (map * %1 %2)
                d/standardize)
              prior
              relative-likelihood-for-grid-for-all-h))

(defn heat-maps [no-of-height-samples]
  [:> sur/Container
   [rc/collapsible
    (r/cursor page-state [:collapsed :prior])
    "Prior"
    [:> react-vega/VegaLite
     {:spec (heat-map-graph "Prior mu = Normal(178,20)"
                            (map #(assoc %1 :ll %2) grid prior)
                            "Prior")}]]
   [:> react-vega/VegaLite
    {:spec
     (hc/xform
      ht/hconcat-chart
      :HCONCAT
      [(heat-map-graph (str "Probability density for " no-of-height-samples " heights.")
                       (map
                        #(assoc %1 :ll %2)
                        grid
                        (nth posteriors no-of-height-samples)))
       (heat-map-graph (str "Relative likelihood of height "
                            (inc no-of-height-samples) " = "
                            (nth adult-heights no-of-height-samples))
                       (map
                        #(assoc %1 :ll %2)
                        grid
                        (nth relative-likelihood-for-grid-for-all-h no-of-height-samples)))
       (heat-map-graph (str "Probability density for " (inc no-of-height-samples) " heights.")
                       (map
                        #(assoc %1 :ll %2)
                        grid
                        (nth posteriors (inc no-of-height-samples))))])}]])

(defn not-reached-sample-limit [no-of-height-samples]
  (< no-of-height-samples (count adult-heights)))

(defn new-random-sample [state]
  (update-in state [:no-of-height-samples] inc))


(defn one-less-sample [state]
  (update-in state [:no-of-height-samples] dec))

(defn play []
  (swap! page-state new-random-sample)
  (swap! page-state assoc-in [:play-timeout-ID]
         (if (not-reached-sample-limit (:no-of-height-samples @page-state))
           (js/setTimeout play (js/Math.floor (/ 1000 (:speed @page-state))))
           nil)))


(defn pause []
  (js/clearTimeout (:play-timeout-ID @page-state))
  (swap! page-state assoc-in [:play-timeout-ID] nil))


(defn buttons []
  (let [tool-tip "Consider new heights"]
    [:div#buttons
     [:> sur/Container
      [:> sur/Popup {:content tool-tip
                     :trigger (r/as-element
                               (if (:play-timeout-ID @page-state)
                                 [:> sur/Button
                                  {:onClick (fn []
                                              (pause))}
                                  [:> sur/Icon {:name "pause"}]]
                                 [:> sur/Button
                                  {:onClick (fn []
                                              (if (not-reached-sample-limit (:samples @page-state))
                                                nil
                                                (swap! page-state assoc-in [:samples] []))
                                              (play))}
                                  [:> sur/Icon {:name "play"}]]))}]
      "Speed : "
      [:input {:type "range" :value (:speed @page-state)  :min 0.5 :max 10 :step 0.5
               :on-change (fn [e]
                            (let [new-value (js/parseFloat (.. e -target -value))]
                              (swap! page-state assoc-in [:speed] new-value)))}]
      (str (:speed @page-state) " samples/second")
      (when (:no-of-height-samples @page-state)
        [:> sur/Button
         {:onClick (fn []
                     (swap! page-state one-less-sample))}
         "Remove a sample"])
      (when (:no-of-height-samples @page-state)
        [:> sur/Button
         {:onClick (fn []
                     (swap! page-state assoc-in [:no-of-height-samples] 0))}
         "Clear samples"])]]))


(defn page []
  [:> sur/Container
   [rc/collapsible
    (r/cursor page-state [:collapsed :question])
    "Question"
    [:> sur/Segment {:raised true}
     [:div [:div.quote
            "\"... we want a single measurement variable to model as a Gaussian distribution. There will be two parameters describing the distribution's shape, the mean mu and the standard deviation sd. Bayesian updating will allow us to consider every possible combination of values for mu and sd and to score each combination by its relative plausibility, in light of the data. These relative plausibilities are the posterior probabilities of each combination of values mu, sd.

Another way to say the above is this. There are an infinite number of possible Gaussian distributions. Some have small means. Others have large means. Some are wide, with a large sd. Others are narrow. We want our Bayesian machine to consider every possible distribution, each defined by a combination of mu and sd, and rank them by posterior plausibility. Posterior plausibility provides a measure of the logical compatibility of each possible distribution with the data and model.\""]
      [:div.attribution "from Richard McElreath's Satistical Rethinking section 4.3"]]]]
   [buttons]
   [heat-maps (:no-of-height-samples @page-state)]])
