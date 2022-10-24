(ns memory-efficiency
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [oz.core :as oz]
            [graphs :as g]))

(defonce app-state (r/atom {:no-of-points 0
                            :play-timeout-ID nil
                            :speed 50}))

(def current-view (atom nil))

(defn store-view! [view]
  (js/console.log "store current view")
  (reset! current-view view))


(def max-no-of-points 10000)

(defonce points (repeatedly max-no-of-points rand))

(defn play []
  (when @current-view
    (js/console.log "calling finalize")
    (.finalize @current-view))
  (swap! app-state (fn [{:keys [no-of-points speed] :as state}]
                     (-> (assoc-in state [:no-of-points] (min (+ no-of-points speed) max-no-of-points))
                         (assoc-in [:play-timeout-ID] (if (< (+ no-of-points speed) max-no-of-points)
                                                        (js/setTimeout play 1000)
                                                        nil))))))

(defn pause []
  (js/clearTimeout (:play-timeout-ID @app-state))
  (swap! app-state assoc-in [:play-timeout-ID] nil))

(defn buttons []
  [:div#buttons
   (if (:play-timeout-ID @app-state)
     [:button
      {:onClick pause}
      "Pause"]
     [:button
      {:onClick (fn []
                  (when (>= (:no-of-points @app-state) max-no-of-points)
                    (swap! app-state assoc-in [:no-of-points] 0))
                  (play))}
      "Play"])
   "Speed : "
   [:select {:value (:speed @app-state)
             :on-change (fn [e]
                          (swap! app-state assoc-in [:speed] (->
                                                              (.. e -target -value)
                                                              js/parseInt)))}
    [:option {:value 1} "1 sample / second"]
    [:option {:value 5} "5 sample / second"]
    [:option {:value 10} "10 sample / second"]
    [:option {:value 50} "50 sample / second"]
    [:option {:value 100} "100 sample / second"]
    [:option {:value 200} "200 sample / second"]
    ]
   (when (< 0 (:no-of-points @app-state))
     [:button
      {:onClick (fn [] (swap! app-state assoc-in [:no-of-points] 0))}
      "Clear samples"])])

(comment
  ;;after at least one graph update
  @current-view
  ;; => #object[View [object Object]]
  )


(defn page []
  [:div
   [buttons]
   [oz/vega-lite
    (g/point-chart
     (g/data (range) (take (:no-of-points @app-state) points))
     (g/titles  (str (:no-of-points @app-state)
                     " points")
                "point number"
                "rand number"))
    {:view-callback store-view!}]])



(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [page]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
