(ns core
  (:require [reagent.dom :as rdom]
            [bayes-update]
            [sampling-from-posterior]
            [reagent.core :as r]
            [semantic-ui-react :as sur]))

(defn page-navigation []
  (r/with-let [showing-page (r/atom 1)]
    [:div
     [:> sur/Menu {:fixed "top"}
      [:> sur/MenuItem {:as "header"} "JointProb"]
      [:> sur/MenuItem {:as "a"
                        :active (== @showing-page 1)
                        :on-click (fn [e]
                                    (.preventDefault e)
                                    (reset! showing-page 1)
                                    nil)}
       "Calculate Posterior Distribution"]
      [:> sur/MenuItem {:as "a" 
                        :active (== @showing-page 2)
                        :on-click (fn [e]
                                    (.preventDefault e)
                                    (reset! showing-page 2)
                                    nil)} 
       "Sampling from Posterior"]]
     [:> sur/Container {:style {:marginTop "7em"}}
      (case @showing-page
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
