(ns react-custom
  (:require [reagent.core :as r]
            [semantic-ui-react :as sur]))



(defn collapsible [heading comp]
  (r/with-let [showing (r/atom false)]
    [:> sur/Accordion {:fluid true}
     (when heading
       [:> sur/Accordion.Title {:index 1
                                :active @showing
                                :on-click (fn [e]
                                            (.preventDefault e)
                                            (swap! showing not)
                                            nil)}
        [:> sur/Icon {:name "dropdown"}] heading])
     (when comp
       [:> sur/Accordion.Content {:active @showing}
        comp])]))