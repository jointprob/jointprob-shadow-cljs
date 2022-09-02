(ns core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [oz.core :as oz]
            [graphs :as g]
            [dbinomial :as d]))

(defonce random-samples (into []  (take 50 (repeatedly (fn [] (if (>= 0.6 (rand)) :w :l))))))
(defonce samples (r/atom '()))

(def grid-p (map #(/ % 200) (range 0 201)))

(defn graph-posterior-dis [samples]
  
  (let [prior-title (if (last samples)
                      "Prior before new sample"
                      "Prior")
        [n land water] (d/count-land-or-water samples)
        [_ last-land last-water] (d/count-land-or-water (butlast samples))
        [_ new-land new-water] (d/count-land-or-water (list (last samples)))
        n-graph (g/land-or-water n land water)
        prior-graph (g/probability-dis prior-title 
                                       (str "Pr(" last-water "," last-land "|p)")
                                       grid-p
                                       (d/r-likelihood-from-samples  grid-p (butlast samples)))
        eq1 (str "Pr(" new-water "," new-land "|p)")
        new-sample-graph (g/probability-dis
                          (str "Likelihood of new sample \"" (last samples) "\" " eq1)
                          eq1
                          grid-p
                          (d/r-likelihood-from-samples grid-p (list (last samples))))

        eq2 (str "Pr(" water "," land "|p)")
        rlikelihood-graph-one-perm (g/probability-dis
                           (str  "Relative Likelihood of This Sequence")
                           eq2
                           grid-p
                           (d/r-likelihood-from-samples-simple grid-p samples))
        rlikelihood-graph-all-perms (g/probability-dis
                                     "Relative Likelihood of Any Sequences"
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
  (< (count samples) (count random-samples)))

(defn one-more-sample [samples]
  (take (inc (count samples)) random-samples))

(defn play []
  (swap! samples one-more-sample)

  (if (more-samples-available @samples)
    (js/setTimeout play 1000)
    nil))

(defn page []
  [:div
   [oz/vega-lite (graph-posterior-dis @samples)]
   [:div
    [:button#play
     {:onClick (fn []
                 (js/console.log "Play pressed")
                 (if (more-samples-available @samples)
                   nil
                   (reset! samples '()))
                 (play))}
     "▶️"]
    (if (more-samples-available @samples)
      [:button
       {:onClick (fn []
                   (js/console.log (str "Next sample - samples " @samples))
                   (swap! samples one-more-sample))}
       (if (more-samples-available @samples) "Next sample" "Clear samples")]
      nil)
    (if (last @samples)
      [:button
       {:onClick (fn []
                   (js/console.log (str "Remove 1 sample " @samples))
                   (swap! samples butlast))}
       "Remove 1 sample"]
      nil)
    (if (last @samples)
      [:button
       {:onClick (fn []
                   (js/console.log (str "Clear samples " @samples))
                   (reset! samples '()))}
       "Clear samples"]
      nil)
    (let [[n land water] (d/count-land-or-water @samples)]
      (str "No. of possible sequences of " water " water sample(s) and " land " land sample(s) : " 
           (d/n-of-permutations water n)))]
    [:div (str "  " @samples)]])

(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [page]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
