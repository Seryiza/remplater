(ns remplater.templates.experiments
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.render :as render]
    [remplater.pdf :as pdf]
    [tick.core :as t])
  (:import
    [org.apache.pdfbox.pdmodel.common PDRectangle]))

(defn document []
  [:document {:output "/tmp/experiments.pdf"
              :page-size pdf/remarkable-2-page-size}
   [:page {:name "big height page"
           :size (PDRectangle. 1404 10000)}
    [:split {:direction :y :splits [1872 1872]}
     [:padding {:padding 10}
      [:rect {:fill nil
              :stroke {:width 1}}]]
     [:padding {:padding 10}
      [:rect {:fill nil
              :stroke {:width 1}}]]
     [:padding {:padding 10}
      [:rect {:fill nil
              :stroke {:width 1}}]]]]

   [:page {:name "text components"}
    [:grid {:rows 10 :cols 1}
     (fn [{:keys [index]}]
       [:rect {:fill {:color (pdf/make-color 200 200 200)}}
        [:text {:text "123123"
                :font-size (* 18 (inc index))}
         [:rect {:fill {:color (pdf/make-color 150 150 150)}}]]])]]])

(comment
  (render/render-document (document)))
