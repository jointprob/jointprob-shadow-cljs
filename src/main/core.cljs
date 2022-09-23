(ns core
  (:require [reagent.dom :as rdom]
            [bayes-update]))

(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [bayes-update/page]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
