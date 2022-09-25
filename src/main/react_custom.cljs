(ns react-custom
  (:require [reagent.core :as r]
            [semantic-ui-react :as sur]))



(defn collapsible [collapsed heading comp]
  [:> sur/Accordion {:fluid true}
   (when heading
     [:> sur/Accordion.Title {:index 1
                              :active (not @collapsed)
                              :on-click (fn [e]
                                          (.preventDefault e)
                                          (swap! collapsed not)
                                          nil)}
      [:> sur/Icon {:name "dropdown"}] heading])
   (when comp
     [:> sur/Accordion.Content {:active (not @collapsed)}
      comp])])