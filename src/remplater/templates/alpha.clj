(ns remplater.templates.alpha
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.render :as render]
    [remplater.pdf :as pdf]
    [tick.core :as t]))

(defn page-layout [attrs & children]
  [:margin {:margin-left 104}
   [:split {:direction :y :splits [104]}
    [:div
     [:split {:direction :x :splits [1000]}
      [:div (:header attrs)]
      [:div (:header-end attrs)]]]
    (into [:div] children)]])

(defn header-button [attrs & children]
  [:margin {:margin 10}
   [:rect {:fill? false
           :stroke? true
           :line-width 5}
    [:text {:text (:title attrs)
            :font-size 80
            :valign :center
            :halign :center}]]])

(defn header-buttons [attrs & children]
  (let [splits (repeat (count children) 100)]
    (into [:split {:direction :x :splits splits}] children)))

(defn daily-page [attrs]
  [:page {:name "daily"}
   [page-layout {:header [:rect
                          [:text {:text "21 September"
                                  :font-size 80
                                  :valign :center
                                  :halign :left}]]
                 :header-end [header-buttons
                              [header-button {:title "i"}]
                              [header-button {:title "n"}]]}
    [:rect]]])

(defn document []
  [:document {:output "/tmp/alpha.pdf"
              :page-size pdf/remarkable-2-page-size
              :fonts {:default "fonts/Alice-Regular.ttf"}}
   [daily-page {}]])

(comment
  (render/render-document (document)))
