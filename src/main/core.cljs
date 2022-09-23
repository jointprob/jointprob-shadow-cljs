(ns core
  (:require [reagent.dom :as rdom]
            [sampling-from-posterior]))

(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [sampling-from-posterior/page]
               (. js/document (getElementById "app"))))

(defn ^:export init [ & args]
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
