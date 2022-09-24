(ns core
  (:require [reagent.dom :as rdom]
            [bayes-update]
            [sampling-from-posterior]
            [reagent.core :as r]
            [semantic-ui-react :as sur]))

(defonce app-state (r/atom {:showing-page 1}))


(defn page-navigation []
  (let [anchor (.. js/window -top -location -hash (substr 1))]
    (swap! app-state assoc-in [:showing-page] (case anchor
                                                "bayes-update" 1
                                                "sampling-from-posterior" 2
                                                1))
    [:div
     [:> sur/Menu {:fixed "top"}
      [:> sur/MenuItem {:as "header"} "JointProb"]
      [:> sur/MenuItem {:as "a"
                        :href "#bayes-update"
                        :active (== (:showing-page @app-state) 1)
                        :on-click (fn [e]
                                    (swap! app-state assoc-in [:showing-page] 1)
                                    nil)}
       "Calculate Posterior Distribution"]
      [:> sur/MenuItem {:as "a"
                        :href "#sampling-from-posterior"
                        :active (== (:showing-page @app-state) 2)
                        :on-click (fn [e]
                                    (swap! app-state assoc-in [:showing-page] 2)
                                    nil)}
       "Sampling from Posterior"]]
     [:> sur/Container {:style {:marginTop "7em"}}
      (case (:showing-page @app-state)
        1 [bayes-update/page]
        2 [sampling-from-posterior/page])]]))


(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [page-navigation]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
