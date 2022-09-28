(ns sampling-from-posterior
  (:require [reagent.core :as r]
            [oz.core :as oz]
            [graphs :as g]
            [dbinomial :as d]
            [semantic-ui-react :as sur]
            [reagent-custom :as rc]))

(defonce samples (repeatedly 100 #(if (>= 0.6 (rand)) :w :l)))

(defonce ten-thousand-pos-dis-samples (repeatedly 1e4 #(d/sample-posterior samples)))

(defonce app-state (r/atom {:pos-dis-samples []
                            :play-timeout-ID nil
                            :speed 50.0
                            :collapsed {:question false
                                        :minimizing-loss-function false
                                        :posterior false}}))

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
   [rc/collapsible 
    (r/cursor app-state [:collapsed :question])
    "Question" 
    [:<>
     [:> sur/Segment {:raised true}
      [:div [:div.quote "\"Suppose I offer you a bet. Tell me which value of p, the 
                        proportion of water on the Earth, you think is correct. I will 
                        pay you $100, if you get it exactly right. But I will subtract 
                        money from your gain, proportional to the distance of your 
                        decision from the correct value. Precisely, your loss is 
                        proportional to the absolute value of d − p, where d is your 
                        decision and p is the correct answer. We could change the precise 
                        dollar values involved, without changing the important aspects of
                        this problem. What matters is that the loss is proportional to 
                        the distance of your decision from the true value.\""]
       [:div.attribution "from Richard McElreath's Satistical Rethinking section 3.2"]]]
     [:div "McElreath doesn't say how many dollars we are penalised but let's say $1 for every 0.01
             error."]]]
    [:> sur/Container
        [rc/collapsible
         (r/cursor app-state [:collapsed :posterior])
         "Posterior Distribution"
         [:<>
          [:div "First let's calculate another posterior distribution calculated from 100 random samples,
           the probability of each of these samples being :w is 0.6 (which is the unobserved parameter 
           we are trying to estimate by Bayesian inference)."]
          [oz/vega-lite (graph-posterior-dis)]]]
         (let [pos-dis (d/posterior-distribution samples)
               loss (d/linear-loss pos-dis)
               [d-for-min-loss
                min-loss] (apply min-key second (zipmap d/grid-p (d/linear-loss (d/posterior-distribution samples))))]
           [:<>
            [rc/collapsible
             (r/cursor app-state [:collapsed :minimizing-loss-function])
             "Minimizing our loss function"
             [:> sur/Segment {:raised true}
              [:div [:p "We want to minimize the absolute difference between d our prediction and the actual p.
                    We can do this in two ways we can work out the minimum loss for the linear loss function abs(d - p)
                    directly from the posterior distribution as follows."]
               [:ul
                [:li "We iterate over values of d from 0 to 1 with a step size of 0.005. And for each value of d:"]
                [:ul
                 [:li "We will iterate over the x-axis of our posterior distribution which is a function that describes the likelihood of values of p between 0 and 1."]
                 [:li "Along the x-axis of our distribution each point on our posterior distribution corresponds to an expected loss for a given d 
              - the absolute value of d- p."]
                 [:li "And the y-axis describes the likelihood of seeing that loss."]
                 [:li "For each point on the curve we multiply the expected loss by the likelihood at that point."]
                 [:li "Then average across the curve to get a full estimate of the expected loss for this d,
               taking into account all the possible values of p described by the posterior distribution and their likelihood."]]]
               [:p (str "This would result in the following plot of exepected losses for the range of possible values of d. With")
                [:strong " a minimum of "
                 (.toFixed min-loss 6) " when d is " d-for-min-loss] "."]]]]

            [oz/vega-lite
             {:layer [(g/line-chart
                       (g/data d/grid-p
                               loss)
                       (g/titles  "Expected Loss"
                                  "decision"
                                  "expected loss"))]}]])
         [:> sur/Segment {:raised true}
          [:div [:p "We can also use samples from the posterior distribution to estimate what value for d will give us the least expected loss.
                    We'll take 10,000 samples from the posterior distribution to simulate what the value of p might be, samples of these p values
                    are in proportion to what inference tells us is likelihood of that value, given the data we have seen."]
           [:p "Theory tells us that the median of these samples will minimize our expected loss, for this linear loss function abs(d - p)."]
           [:p "Press play below to see an animation of taking up to 2,000 samples from the posterior. Hopefully this may aid understanding of what these samples are."]]]
         [buttons]
         [pos-dis-samples-graph (:pos-dis-samples @app-state)]
         [:img {:src "/imgs/1e4samples.png"}]

         (let [this-median (.toFixed (d/median ten-thousand-pos-dis-samples) 6)
               loss (js/Math.round (* 100 (abs (- 0.6 this-median))))]
           [:> sur/Segment {:raised true}
            [:p [:strong "The median sample of the collection of ten thousand samples is "
                 this-median
                 " to 6 significant figures."]
             " The median sample in this scenario should minimize our potential losses."
             " If we say we lose $1 for every 0.01 we are away from the 
             correct answer then we will have lost (0.6 - " this-median ") * 100 = $"
             loss "."]
            [:p (str "And we are left with the $100 McElreath will give us minus the loss. $100 - " loss " = $" (- 100 loss) ".")]])]])
   

(comment
  (d/round-number-to-grid(nth (sort ten-thousand-pos-dis-samples)
                          (js/Math.floor( / (count ten-thousand-pos-dis-samples) 2)))) 

  (g/point-chart
   (g/size-of-mark 25)
   (g/data [0] [1])
   (g/titles  "Count of Samples in 200 Bins"
              "% of world that is water"
              "Count"))
  samples
  (apply min-key second (zipmap d/grid-p (d/linear-loss (d/posterior-distribution samples)))))
  