(ns remplater.templates.experiments
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.render :as render]
    [remplater.pdf :as pdf]
    [tick.core :as t]))

(defn document []
  [:document {:output "/tmp/experiments.pdf"
              :page-size pdf/remarkable-2-page-size}
   [:page {:name "text components"}
    [:grid {:rows 10 :cols 1}
     (fn [{:keys [index]}]
       [:rect {:fill-color (pdf/make-color 200 200 200)}
        [:text {:text "123123"
                :font-size (* 18 (inc index))}
         [:rect {:fill-color (pdf/make-color 150 150 150)}]]])]]])

(comment
  (render/render-document (document)))
