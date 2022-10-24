(ns bayes-update
  (:require [dbinomial :as d]
            [graphs :as g]
            [mathjax-react :as mj]
            [react-vega]
            [reagent.core :as r]
            [semantic-ui-react :as sur]
            [reagent-custom :as rc]))


(defonce page-state (r/atom {:prior "Uniform"
                            :samples []
                            :play-timeout-ID nil
                            :speed 1.0
                            :collapsed
                            {:question false
                             :prior true
                             :formulae true}}))


(def priors {"Uniform" (repeat 201 1)
             "Step up" (map #(if (<= % 0.5) 0 2) d/grid-p)
             "Step down" (map #(if (<= % 0.5) 2 0) d/grid-p)
             "Ramp up and down" (-> (map #(if (<= % 0.5) (d/exp 1.08 (* 200 %)) (d/exp 1.08 (* 200 (- 1 %)))) d/grid-p)
                                    (d/standardize))})

(defn graph-posterior-dis [samples]
  (let [prior-title (if (last samples)
                      "Prior before new sample"
                      "Prior")
        [n land water] (d/count-land-or-water samples)
        [_ last-land last-water] (d/count-land-or-water (butlast samples))
        [_ new-land new-water] (d/count-land-or-water (list (last samples)))
        n-graph
        (g/bar-chart
         (g/titles (str "n = " n)  "Land (L) or Water (W)?" "Percent of Sample")
         {:data {:values
                 [{:x "W" :y (if (zero? n) 0 (/ water n))}
                  {:x "L" :y (if (zero? n) 0 (/ land n))}]}}
         (g/percentage-axis :y))
        prior-before-this-sample-graph
        (g/probability-dis
         (g/data d/grid-p (d/posterior-distribution
                           (butlast samples)
                           (get priors (:prior @page-state))))
         (g/titles prior-title
                   "% of world that is water"
                   (str "Pr(" last-water "," last-land " ⎹ p)")))
        eq1 (str "Pr(" new-water "," new-land " ⎹ p)")
        new-sample-graph
        (g/probability-dis
         (g/data d/grid-p
                 (d/relative-likelihood-from-samples (list (last samples))))
         (g/titles (str "Probability of new sample \"" (last samples) "\" " eq1)
                   "% of world that is water"
                   eq1))

        eq2 (str "Pr(" water "," land " ⎹ p)")
        rlikelihood-graph-one-perm
        (g/probability-dis
         (g/data d/grid-p
                 (d/r-likelihood-from-samples-for-this-sequence samples))
         (g/titles (str  "Probability of This (" water "," land ") Sequence")
                   "% of world that is water"
                   eq2)
         (g/axis-format :y ".3e"))
        rlikelihood-graph-all-perms
        (g/probability-dis
         (g/data
          d/grid-p
          (d/relative-likelihood-from-samples samples))
         (g/titles (str "Probability of Any (" water "," land ") Sequence Pr(W,L ⎹ p)")
                   "% of world that is water"
                   eq2))
        pos-graph
        (g/probability-dis
         (g/data d/grid-p (d/posterior-distribution
                           samples
                           (get priors (:prior @page-state))))
         (g/titles  "Posterior Pr(p ⎹ W,L)"
                    "% of world that is water"
                    "Pr(p ⎹ W,L)"))]
    {:vconcat
     (if (last samples)
       [{:hconcat [n-graph prior-before-this-sample-graph new-sample-graph]}
        {:hconcat [rlikelihood-graph-one-perm rlikelihood-graph-all-perms pos-graph]}]
       [{:hconcat [n-graph prior-before-this-sample-graph]}
        {:hconcat [rlikelihood-graph-one-perm rlikelihood-graph-all-perms pos-graph]}])}))

;; functionality to respond to button presses

(def max-samples 200)

(defn not-reached-sample-within-10-of-limit [samples]
  (<= (+ (count samples) 10) max-samples))

(defn not-reached-sample-limit [samples]
  (< (count samples) max-samples))

(defn new-random-sample [{:keys [samples] :as state}]
  (assoc-in state [:samples] (conj samples (if (>= 0.6 (rand)) :w :l))))

(defn ten-new-random-samples [state]
  (reduce (fn [arg1 _] (new-random-sample arg1)) state (range 10)))

(defn user-sample [{:keys [samples] :as state} water-or-land]
  (assoc-in state [:samples] (conj samples water-or-land)))


(defn one-less-sample [{:keys [samples] :as state}]
  (assoc-in state [:samples] (into [] (butlast samples))))

(defn ten-less-samples [{:keys [samples] :as state}]
  (if (> (count samples) 10)
    (assoc-in state [:samples] (into [] (drop-last 10 samples)))
    (assoc-in state [:samples] (vector))))


(defn play []
  (swap! page-state new-random-sample)
  (swap! page-state assoc-in [:play-timeout-ID]
         (if (not-reached-sample-limit (:samples @page-state))
           (js/setTimeout play (js/Math.floor (/ 1000 (:speed @page-state))))
           nil)))


(defn pause []
  (js/clearTimeout (:play-timeout-ID @page-state))
  (swap! page-state assoc-in [:play-timeout-ID] nil))


(defn buttons []
  (let [tool-tip "Randomly generated samples are :w with a probability of 0.6."]
    [:div#buttons
     [:> sur/Container 
      [:> sur/Popup {:content tool-tip
                     :trigger (r/as-element
                               (if (:play-timeout-ID @page-state)
                                 [:> sur/Button
                                  {:onClick (fn []
                                              (js/console.log "Pause pressed")
                                              (pause))}
                                  [:> sur/Icon {:name "pause"}]]
                                 [:> sur/Button
                                  {:onClick (fn []
                                              (js/console.log "Play pressed")
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
      (when (not-reached-sample-limit (:samples @page-state))
        [:> sur/Popup {:content tool-tip
                       :trigger (r/as-element
                                 [:> sur/Button
                                  {:onClick (fn [] (swap! page-state new-random-sample))}
                                  "Random sample"])}])
      (when (not-reached-sample-within-10-of-limit (:samples @page-state))
        [:> sur/Popup {:content tool-tip
                       :trigger (r/as-element
                                 [:> sur/Button
                                  {:onClick (fn [] (swap! page-state ten-new-random-samples))}
                                  "x 10"])}])
      (when (not-reached-sample-limit (:samples @page-state))
        [:> sur/Button
         {:onClick (fn [] (swap! page-state user-sample :w))}
         ":w"])
      (when (not-reached-sample-limit (:samples @page-state))
        [:> sur/Button
         {:onClick (fn [] (swap! page-state user-sample :l))}
         ":l"])]
     [:> sur/Container 
      (when (last (:samples @page-state))
        [:> sur/Button
         {:onClick (fn []
                     (js/console.log (str "Remove 1 sample " (:samples @page-state)))
                     (swap! page-state one-less-sample))}
         "Remove a sample"])
      (when (>= (count (:samples @page-state)) 10)
        [:> sur/Button
         {:onClick (fn [] (swap! page-state ten-less-samples))}
         "x 10"])
      (when (last (:samples @page-state))
        [:> sur/Button
         {:onClick (fn []
                     (js/console.log (str "Clear samples " (:samples @page-state)))
                     (swap! page-state assoc-in [:samples] []))}
         "Clear samples"])]]))


(defn page []
  (let [[n land water] (d/count-land-or-water (:samples @page-state))
        permutations (str "Number of possible sequences of "
                          water
                          " water sample"
                          (if (= water 1) "" "s")
                          " and "
                          land
                          " land sample"
                          (if (= land 1) "" "s")
                          " = ")]
    [:> sur/Container
     [rc/collapsible
      (r/cursor page-state [:collapsed :question])
      "Question"
      [:> sur/Segment {:raised true}
       [:div [:div.quote 
      "\"Suppose you have a globe representing our planet, the Earth. This version of the 
       world is small enough to hold in your hands. You are curious how much of the 
       surface is covered in water. You adopt the following strategy: You will toss the 
       globe up in the air. When you catch it, you will record whether or not the surface 
       under your right index finger is water or land. Then you toss the globe up in the 
       air again and repeat the procedure. This strategy generates a sequence of samples 
       from the globe.\""]
        [:div.attribution "from Richard McElreath's Satistical Rethinking section 2.2"]]]]
     [rc/collapsible
      (r/cursor page-state [:collapsed :prior])
      "Prior"
      [:div

       [:label "Prior before any data "]
       (into (vector)
             (concat [:select.form-control {:field :list
                                            :value (:prior @page-state)
                                            :id :many.options
                                            :on-change #(swap! page-state assoc-in
                                                               [:prior]
                                                               (.. % -target -value))}]
                     (map #(vector :option {:key (first %)} (first %)) priors)))
       [:div
        [:> react-vega/VegaLite
         {:spec
          (g/probability-dis
           (g/data d/grid-p
                   (get priors (:prior @page-state)))
           (g/titles "Prior before any data"
                     "% of world that is water"
                     "Pr(p)"))}]]]]
     [buttons]
     [rc/collapsible
      (r/cursor page-state [:collapsed :formulae])
       "Formulae"
       [:div
        [:p (str "Samples:  " (:samples @page-state))]
        [:p "W = " water " ;  L = " land]
        [:p
         "Probability of this (W,L) sequence of samples = "
         [:> mj/MathComponent {:tex "p^{W}(1-p)^L" :display false}]
         " = "
         [:> mj/MathComponent {:tex (str "p^{" water "}(1-p)^{" land "}") :display false}]]
        [:p
         permutations
         [:> mj/MathComponent
          {:tex "\\frac{(W+L)!}{W!L!}" :display false}]
         " = "
         [:> mj/MathComponent
          {:tex (str "\\frac{(" water "+" land ")!}{" water "!" land "!}") :display false}]
         " = "
         (d/n-of-permutations water n)]
        [:p
         "Probability of any of " (d/n-of-permutations water n) " (W,L) sequence(s) = "
         [:> mj/MathComponent {:tex "Pr(W,L|p)=\\frac{(W+L)!}{W!L!}p^{W}(1-p)^L" :display false}]
         " = "
         [:> mj/MathComponent {:tex (str "\\frac{(" water "+" land ")!}{" water "!" land "!} "
                                         "p^{" water "}(1-p)^{" land "}") :display false}]
         " = "
         (d/n-of-permutations water n)
         " "
         [:> mj/MathComponent {:tex (str "p^{" water "}(1-p)^{" land "}") :display false}]]
        [:p
         "Posterior = "
         [:> mj/MathComponent {:tex "Pr(p \\mid W, L)= 
                                     \\frac{\\text { Probability of the data } \\times 
                                     \\text { Prior }}{\\text { Average probability of the data}} = " :display false}]

         [:> mj/MathComponent {:tex "\\frac{Pr(W, L \\mid p) Pr(p)}{Pr(W, L)}" :display false}]
         " where "
         [:> mj/MathComponent {:tex "\\text { Average probability of the data} = Pr(W, L) = \\int _0 ^1 {Pr(W, L \\mid p) Pr(p)}dp" :display false}]]]]
     [:> react-vega/VegaLite
      {:spec (graph-posterior-dis (:samples @page-state))}]]))
