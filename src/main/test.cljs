(ns test
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [oz.core :as oz]))

(defn car-chart []
  [:div
   [oz/vega-lite {:data {:url "data/cars.json"},
                  :mark "point"
                 :width 400,
                 :height 300,
                 :background "floralwhite",
                 :encoding
                 {:x {:field "displ", :type "quantitative"},
                  :y {:field "hwy", :type "quantitative"},
                  :color {:field "manufacturer", :type "nominal"},
                  :tooltip
                  [{:field "displ", :type "quantitative"}
                   {:field "hwy", :type "quantitative"}]}}]])

(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [car-chart]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
