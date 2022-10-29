(ns bayes-main-page
  (:require [reagent.dom :as rdom]
            [bayes-update]
            [sampling-from-posterior]
            [normal-distribution]
            [reagent.core :as r]
            [semantic-ui-react :as sur]))

(defonce navigation-state (r/atom {:showing-page 1}))

(defn display [bool comp]
  [:div {:style {:display (if bool "block" "none")}} comp])

(defn page-navigation []
  (let [anchor (.. js/window -top -location -hash (substr 1))]
    (swap! navigation-state assoc-in [:showing-page] (case anchor
                                                "bayes-update" 1
                                                "sampling-from-posterior" 2
                                                "normal-distribution" 3
                                                1))
    [:div
     [:> sur/Menu {:fixed "top" :pointing true}
      [:> sur/MenuItem {:as "a" :href "https://scicloj.github.io/docs/community/groups/jointprob/"}
       [:> sur/Icon {:name "external"}] "JointProb Study Group"]
      [:> sur/MenuItem {:as "a"
                        :href "#bayes-update"
                        :active (== (:showing-page @navigation-state) 1)
                        :on-click (fn [e]
                                    (swap! navigation-state assoc-in [:showing-page] 1)
                                    nil)}
       "Calculate Posterior Distribution"]
      [:> sur/MenuItem {:as "a"
                        :href "#sampling-from-posterior"
                        :active (== (:showing-page @navigation-state) 2)
                        :on-click (fn [e]
                                    (swap! navigation-state assoc-in [:showing-page] 2)
                                    nil)}
       "Sampling from Posterior"]
      [:> sur/MenuItem {:as "a"
                        :href "#normal-distribution"
                        :active (== (:showing-page @navigation-state) 3)
                        :on-click (fn [e]
                                    (swap! navigation-state assoc-in [:showing-page] 3)
                                    nil)}
       "Normal Distribution"]]
     [:> sur/Container {:style {:marginTop "7em"}}
      (let [current-page (:showing-page @navigation-state)]
        [:<> 
         [display (==  current-page 1) [bayes-update/page]]
         [display (== current-page 2) [sampling-from-posterior/page]]
         [display (== current-page 3) [normal-distribution/page]]])]]))


(defn ^:dev/after-load start []
  (js/console.log "start")
  (rdom/render [page-navigation]
               (. js/document (getElementById "app"))))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
