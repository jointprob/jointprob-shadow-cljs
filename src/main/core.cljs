(ns core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [oz.core :as oz]
            [graphs :as g]
            [dbinomial :as d]
            [mathjax-react :as mj]
            [semantic-ui-react :as sur]))

(defonce app-state (r/atom {:prior "Uniform"
                            :samples []
                            :play-timeout-ID nil
                            :speed 1000}))


(def grid-p (map #(/ % 200) (range 0 201)))

(def priors {"Uniform" (repeat 201 1)
             "Step up" (map #(if (<= % 0.5) 0 2) grid-p)
             "Step down" (map #(if (<= % 0.5) 2 0) grid-p)
             "Ramp up and down" (-> (map #(if (<= % 0.5) (d/exp 1.08 (* 200 %)) (d/exp 1.08 (* 200 (- 1 %)))) grid-p)
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
         (g/data grid-p
                 (->> (d/r-likelihood-from-samples  grid-p (butlast samples))
                      (map * (get priors (:prior @app-state)))
                      d/standardize))
         (g/titles prior-title
                   "% of world that is water"
                   (str "Pr(" last-water "," last-land " ⎹ p)")))
        eq1 (str "Pr(" new-water "," new-land " ⎹ p)")
        new-sample-graph
        (g/probability-dis
         (g/data grid-p
                 (d/r-likelihood-from-samples grid-p (list (last samples))))
         (g/titles (str "Probability of new sample \"" (last samples) "\" " eq1)
                   "% of world that is water"
                   eq1))

        eq2 (str "Pr(" water "," land " ⎹ p)")
        rlikelihood-graph-one-perm
        (g/probability-dis
         (g/data grid-p
                 (d/r-likelihood-from-samples-for-this-sequence grid-p samples))
         (g/titles (str  "Probability of This (" water "," land ") Sequence")
                   "% of world that is water"
                   eq2)
         (g/axis-format :y ".3e"))
        rlikelihood-graph-all-perms
        (g/probability-dis
         (g/data
          grid-p
          (d/r-likelihood-from-samples grid-p samples))
         (g/titles (str "Probability of Any (" water "," land ") Sequence Pr(W,L ⎹ p)")
                   "% of world that is water"
                   eq2))
        pos-graph
        (g/probability-dis
         (g/data grid-p
                 (->>
                  (d/r-likelihood-from-samples grid-p samples)
                  (map * (get priors (:prior @app-state)))
                  d/standardize))
         (g/titles  "Posterior Pr(p ⎹ W,L)"
                    "% of world that is water"
                    "Pr(p ⎹ W,L)"))]
    {:vconcat
     (if (last samples)
       [{:hconcat [n-graph prior-before-this-sample-graph new-sample-graph]}
        {:hconcat [rlikelihood-graph-one-perm rlikelihood-graph-all-perms pos-graph]}]
       [{:hconcat [n-graph prior-before-this-sample-graph]}
        {:hconcat [rlikelihood-graph-one-perm rlikelihood-graph-all-perms pos-graph]}])}))

(def max-samples 200)

(defn not-reached-sample-within-10-of-limit [samples]
  (<= (+ (count samples) 10) max-samples))

(defn not-reached-sample-limit [samples]
  (< (count samples) max-samples))

(defn new-random-sample [{:keys [samples] :as state}]
  (assoc-in state [:samples] (conj samples (if (>= 0.6 (rand)) :w :l))))

(defn ten-new-random-samples [{:keys [samples] :as state}]
  (assoc-in state [:samples] (into [] (concat samples (repeatedly 10 #(if (>= 0.6 (rand)) :w :l))))))


(defn user-sample [{:keys [samples] :as state} water-or-land]
  (assoc-in state [:samples] (conj samples water-or-land)))


(defn one-less-sample [{:keys [samples] :as state}]
  (assoc-in state [:samples] (into [] (butlast samples))))

(defn ten-less-samples [{:keys [samples] :as state}]
  (if (> (count samples) 10)
    (assoc-in state [:samples] (into [] (drop-last 10 samples)))
    (assoc-in state [:samples] (vector))))


(defn play []
  (swap! app-state new-random-sample)
  (swap! app-state assoc-in [:play-timeout-ID]
         (if (not-reached-sample-limit (:samples @app-state))
           (js/setTimeout play (:speed @app-state))
           nil)))


(defn pause []
  (js/clearTimeout (:play-timeout-ID @app-state))
  (swap! app-state assoc-in [:play-timeout-ID] nil))


(defn buttons []
  (let [tool-tip "Randomly generated samples are :w with a probability of 0.6."]
    [:div#buttons
     [:> sur/Popup {:content tool-tip
                    :trigger (r/as-element
                              (if (:play-timeout-ID @app-state)
                                [:> sur/Button
                                 {:onClick (fn []
                                             (js/console.log "Pause pressed")
                                             (pause))}
                                 [:> sur/Icon {:name "pause"}]]
                                [:> sur/Button
                                 {:onClick (fn []
                                             (js/console.log "Play pressed")
                                             (if (not-reached-sample-limit (:samples @app-state))
                                               nil
                                               (swap! app-state assoc-in [:samples] []))
                                             (play))}
                                 [:> sur/Icon {:name "play"}]]))}]
     "Speed : "
     [:input {:type "range" :value (:speed @app-state) :min 125 :max 2000 :step 125
              :tooltip (str " - new sample every " (/ (:speed @app-state) 1000) " seconds")
              :on-change (fn [e]
                           (let [new-value (js/parseInt (.. e -target -value))]
                             (swap! app-state assoc-in [:speed] new-value)))}]
     (str (.toFixed (/ 1000 (:speed @app-state)) 2) " samples/second")
     (when (not-reached-sample-limit (:samples @app-state))
       [:> sur/Popup {:content tool-tip
                      :trigger (r/as-element
                                [:> sur/Button
                                 {:onClick (fn [] (swap! app-state new-random-sample))}
                                 "Random sample"])}])
     (when (not-reached-sample-within-10-of-limit (:samples @app-state))
       [:> sur/Popup {:content tool-tip
                      :trigger (r/as-element
                                [:> sur/Button
                                 {:onClick (fn [] (swap! app-state ten-new-random-samples))}
                                 "x 10"])}])
     (when (not-reached-sample-limit (:samples @app-state))
       [:> sur/Button
        {:onClick (fn [] (swap! app-state user-sample :w))}
        ":w"])
     (when (not-reached-sample-limit (:samples @app-state))
       [:> sur/Button
        {:onClick (fn [] (swap! app-state user-sample :l))}
        ":l"])
     (when (last (:samples @app-state))
       [:> sur/Button
        {:onClick (fn []
                    (js/console.log (str "Remove 1 sample " (:samples @app-state)))
                    (swap! app-state one-less-sample))}
        "Remove a sample"])
     (when (>= (count (:samples @app-state)) 10)
       [:> sur/Button
        {:onClick (fn [] (swap! app-state ten-less-samples))}
        "x 10"])
     (when (last (:samples @app-state))
       [:> sur/Button
        {:onClick (fn []
                    (js/console.log (str "Clear samples " (:samples @app-state)))
                    (swap! app-state assoc-in [:samples] []))}
        "Clear samples"])]))


(defn collapsible [{:keys [comp heading]}]
  (r/with-let [showing (r/atom false)]
    [:> sur/Accordion {:defaultActiveIndex 0 :fluid true}
     (when heading
       [:> sur/Accordion.Title {:index 1
                                :active @showing
                                :on-click (fn [e]
                                            (.preventDefault e)
                                            (swap! showing not)
                                            nil)} 
        [:> sur/Icon {:name "dropdown"}] heading])
     (when comp
       [:> sur/Accordion.Content {:active @showing}
        comp])]))

(defn page []
    (let [[n land water] (d/count-land-or-water (:samples @app-state))
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
       [collapsible {:heading "Prior"
                     :comp
                     [:div

                      [:label "Prior before any data "]
                      (into (vector) (concat [:select.form-control {:field :list
                                                                    :value (:prior @app-state)
                                                                    :id :many.options
                                                                    :on-change #(swap! app-state assoc-in [:prior] (.. % -target -value))}]
                                             (map #(vector :option {:key (first %)} (first %)) priors)))
                      [:div
                       [oz/vega-lite (g/probability-dis
                                     (g/data grid-p
                                             (get priors (:prior @app-state)))
                                     (g/titles "Prior before any data"
                                               "% of world that is water"
                                               "Pr(p)"))]]]}]
       [buttons]
       [collapsible
        {:heading "Formulae"
         :comp
         [:div
          [:p (str "Samples:  " (:samples @app-state))]
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
           [:> mj/MathComponent {:tex "\\text { Average probability of the data} = Pr(W, L) = \\int _0 ^1 {Pr(W, L \\mid p) Pr(p)}dp" :display false}]]]}]
        [oz/vega-lite (graph-posterior-dis (:samples @app-state))]]))

(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [page]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
