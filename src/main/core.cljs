(ns core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [oz.core :as oz]
            [graphs :as g]
            [dbinomial :as d]))

(defonce app-state (r/atom {:samples []
                            :play-timeout-ID nil}))

(def grid-p (map #(/ % 200) (range 0 201)))

(defn graph-posterior-dis [samples]
  (let [prior-title (if (last samples)
                      "Prior before new sample"
                      "Prior")
        [n land water] (d/count-land-or-water samples)
        [_ last-land last-water] (d/count-land-or-water (butlast samples))
        [_ new-land new-water] (d/count-land-or-water (list (last samples)))
        n-graph (g/land-or-water n land water)
        prior-graph (g/probability-dis
                     prior-title
                     (str "Pr(" last-water "," last-land "|p)")
                     grid-p
                     (-> (d/r-likelihood-from-samples  grid-p (butlast samples))
                         d/standardize))
        eq1 (str "Pr(" new-water "," new-land "|p)")
        new-sample-graph (g/probability-dis
                          (str "Probability of new sample \"" (last samples) "\" " eq1)
                          eq1
                          grid-p
                          (d/r-likelihood-from-samples grid-p (list (last samples))))

        eq2 (str "Pr(" water "," land "|p)")
        rlikelihood-graph-one-perm
        (g/probability-dis
         (str  "Probability of This (" water "," land ") Sequence")
         eq2
         grid-p
         (d/r-likelihood-from-samples-simple grid-p samples))
        rlikelihood-graph-all-perms
        (g/probability-dis
         (str "Probability of Any (" water "," land ") Sequence")
         eq2
         grid-p
         (d/r-likelihood-from-samples grid-p samples))
        pos-graph (g/probability-dis "Posterior Probability (standardized)"
                                     "Pr(p|W,L)" grid-p
                                     (->
                                      (d/r-likelihood-from-samples grid-p samples)
                                      d/standardize))]
    {:vconcat
     (if (last samples)
       [{:hconcat [n-graph prior-graph new-sample-graph]}
        {:hconcat [rlikelihood-graph-one-perm rlikelihood-graph-all-perms pos-graph]}]
       [{:hconcat [n-graph prior-graph]}
        {:hconcat [rlikelihood-graph-one-perm rlikelihood-graph-all-perms pos-graph]}])}))


(defn more-samples-available [samples]
  (< (count samples) 100))

(defn one-more-sample [{:keys [samples] :as state}]
  (assoc-in state [:samples] (conj samples (if (>= 0.6 (rand)) :w :l))))

(defn one-less-sample [{:keys [samples] :as state}]
  (assoc-in state [:samples] (into [] (butlast samples))))

(defn play []
  (swap! app-state one-more-sample)

  (if (more-samples-available (:samples @app-state))
    (swap! app-state assoc-in [:play-timeout-ID] (js/setTimeout play 1000))
    nil))


(defn pause []
  (js/clearTimeout (:play-timeout-ID @app-state))
  (swap! app-state assoc-in [:play-timeout-ID] nil))

(defn page []
  [:div
   [:div
    [:img {:src "imgs/posterior-eq.png" :width "45%"}]
    [:img {:src "imgs/binomial-eq.png" :width "45%"}]]
   [oz/vega-lite (graph-posterior-dis (:samples @app-state))]
   [:div
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
    (if (more-samples-available (:samples @app-state))
      [:button
       {:onClick (fn []
                   (js/console.log (str "New sample - samples " (:samples @app-state)))
                   (swap! app-state one-more-sample))}
       (if (more-samples-available (:samples @app-state)) "New sample" "Clear samples")]
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
    (let [[n land water] (d/count-land-or-water (:samples @app-state))]
      (str "No. of possible sequences of " water " water sample(s) and " land " land sample(s) : "
           (d/n-of-permutations water n)))]
   [:div (str "  " (:samples @app-state))]])

(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [page]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
