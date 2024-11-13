(ns remplater.templates.alpha
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.render :as render]
    [remplater.pdf :as pdf]
    [tick.core :as t]))

(def timeline-pattern
  {:width 150
   :height 75
   :row (fn [{:keys [row-index rows start-hour]}]
          (let [hour? (even? row-index)
                hour (rem (+ start-hour (/ row-index 2)) 24)]
            [:split {:direction :x :splits [50 #(- % (/ (:width timeline-pattern) 2))]}
             [:div (when hour?
                     [:margin {:margin-right 10}
                      [:text {:text (str hour)
                              :font-size 25
                              :valign :center
                              :halign :right}]])]
             [:line {:color (if hour?
                              (pdf/make-color 100 100 100)
                              (pdf/make-color 0 0 0))
                     :line-type :horizontal-middle
                     :dash (when-not hour?
                             {:pattern [1 3]
                              :phase 0})}]]))
   :col (fn [{:keys [col-index cols]}]
          [:margin {:margin-top (/ (:height timeline-pattern) 2)
                    :margin-bottom (/ (:height timeline-pattern) 2)}
           [:line {:line-type :vertical-middle}]])})

(def cells-pattern
  {:width 45
   :height 45
   :line (fn [{:keys [col-index row-index]}]
           (let [strong-line? (= 7 row-index)]
             [:line {:color (if strong-line?
                                (pdf/make-color 0 0 0)
                                (pdf/make-color 100 100 100))
                     :width (if strong-line? 4 1)}]))
   :outline [:line {:color (pdf/make-color 100 100 100)}]})

(defn page-layout [attrs & children]
  [:margin {:margin-left 104}
   [:split {:direction :y :splits [104]}
    [:div
     [:split {:direction :x :splits [1000]}
      [:div (:header attrs)]
      [:div (:header-end attrs)]]]
    (into [:div] children)]])

(defn header-button [attrs & children]
  [:margin {:margin 30}
   [:circle {:fill? false
             :stroke? true
             :line-width 4
             :radius 20}
    [:text {:text (:title attrs)
            :font-size 30
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
    [:split {:direction :x :splits [#(/ % 2)]}
     [:margin {:margin-top 40
               :margin-bottom 40}
      [:pattern-grid {:pattern cells-pattern}]]
     [:pattern-grid {:pattern timeline-pattern
                     :start-hour 10}]]]])

(defn document []
  [:document {:output "/tmp/alpha.pdf"
              :page-size pdf/remarkable-2-page-size
              :fonts {:default "fonts/Alice-Regular.ttf"}}
   [daily-page {}]])

(comment
  (render/render-document (document)))
