(ns core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [oz.core :as oz]
            [graphs :as g]
            [dbinomial :as d]
            [mathjax-react :as mj]))

(defonce app-state (r/atom {:samples []
                            :play-timeout-ID nil
                            :speed 1000}))


(def grid-p (map #(/ % 200) (range 0 201)))

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
                 (-> (d/r-likelihood-from-samples  grid-p (butlast samples))
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
                 (->
                  (d/r-likelihood-from-samples grid-p samples)
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


(defn more-samples-available [samples]
  (< (count samples) 200))

(defn new-random-sample [{:keys [samples] :as state}]
  (assoc-in state [:samples] (conj samples (if (>= 0.6 (rand)) :w :l))))

(defn user-sample [{:keys [samples] :as state} water-or-land]
  (assoc-in state [:samples] (conj samples water-or-land)))


(defn one-less-sample [{:keys [samples] :as state}]
  (assoc-in state [:samples] (into [] (butlast samples))))

(defn play []
  (swap! app-state new-random-sample)
  (swap! app-state assoc-in [:play-timeout-ID]
         (if (more-samples-available (:samples @app-state))
           (js/setTimeout play (:speed @app-state))
           nil)))


(defn pause []
  (js/clearTimeout (:play-timeout-ID @app-state))
  (swap! app-state assoc-in [:play-timeout-ID] nil))


(defn buttons []
  [:div#buttons
   (if (:play-timeout-ID @app-state)
     [:button#pause
      {:onClick (fn []
                  (js/console.log "Pause pressed")
                  (pause))}
      "⏸"]
     [:button#play
      {:onClick (fn []
                  (js/console.log "Play pressed")
                  (if (more-samples-available (:samples @app-state))
                    nil
                    (swap! app-state assoc-in [:samples] []))
                  (play))}
      "▶️"])
   "Speed : "
   [:input {:type "range" :value (:speed @app-state) :min 125 :max 2000 :step 125
            :tooltip (str " - new sample every " (/ (:speed @app-state) 1000) " seconds")
            :on-change (fn [e]
                         (let [new-value (js/parseInt (.. e -target -value))]
                           (swap! app-state assoc-in [:speed] new-value)))}]
   (str (.toFixed (/ 1000 (:speed @app-state)) 2) " samples/second")
   (if (more-samples-available (:samples @app-state))
     [:button
      {:onClick (fn []
                  (js/console.log (str "New sample - samples " (:samples @app-state)))
                  (swap! app-state new-random-sample))}
      "Random sample"]
     nil)
   (if (more-samples-available (:samples @app-state))
     [:button
      {:onClick (fn []
                  (js/console.log (str "New :w sample - samples " (:samples @app-state)))
                  (swap! app-state user-sample :w))}
      ":w"]
     nil)
   (if (more-samples-available (:samples @app-state))
     [:button
      {:onClick (fn []
                  (js/console.log (str "New :w sample - samples " (:samples @app-state)))
                  (swap! app-state user-sample :l))}
      ":l"]
     nil)
   (if (last (:samples @app-state))
     [:button
      {:onClick (fn []
                  (js/console.log (str "Remove 1 sample " (:samples @app-state)))
                  (swap! app-state one-less-sample))}
      "Remove a sample"]
     nil)
   (if (last (:samples @app-state))
     [:button
      {:onClick (fn []
                  (js/console.log (str "Clear samples " (:samples @app-state)))
                  (swap! app-state assoc-in [:samples] []))}
      "Clear samples"]
     nil)
   " Randomly generated samples are :w with a probability of 0.6."])

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
      
      [:div
       [buttons]
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
         [:> mj/MathComponent {:tex "\\text { Average probability of the data} = Pr(W, L) = \\int _0 ^1 {Pr(W, L \\mid p) Pr(p)}dp" :display false}]]


        [oz/vega-lite (graph-posterior-dis (:samples @app-state))]]]))

(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [page]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
