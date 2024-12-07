(ns remplater.templates.autofocus
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.render :as render]
    [remplater.datetime :as dt]
    [remplater.pdf :as pdf]
    [tick.core :as t])
  (:import
    [org.apache.pdfbox.pdmodel.common PDRectangle]))

(def color-light-gray (pdf/make-color 100 100 100))

(defn get-note-page-name [note-index] (str "note-" note-index))

(def index-cells-pattern
  {:width "10%"
   :height 80
   :row-count 20
   :outline [:line {:width 1}]
   :line [:line {:width 1}]
   :cell (fn [{:keys [index]} & children]
           [:page-link {:target-page (get-note-page-name index)}
            [:padding {:padding-left 20}
             [:split {:direction :x :splits [20]}
              [:circle {:radius 12}]
              [:padding {:padding 10}
               [:text {:text (-> index inc str)
                       :font-size 20
                       :valign :center
                       :halign :left}]]]]])})

(def note-pattern
  {:width "50%"
   :height 52
   :row-count 35
   :outline (fn [{:as attrs :keys [side]} & children]
              (when (#{:bottom :top} side)
                [:line {:width 1
                        :color color-light-gray}]))
   :line [:line {:width 1
                 :color color-light-gray}]})

(defn index-page [{:keys [notes-count]}]
  [:page {:name "index"}
   [:padding {:padding 120}
    [:pattern-grid {:pattern index-cells-pattern}]]])

(defn note-page [{:keys [note-index]}]
  [:page {:name (get-note-page-name note-index)
          :size (PDRectangle. 1404 4000)}
   [:split {:direction :y :splits [105]}
    [:padding {:padding-left 120}
     [:page-link {:target-page "index"}
      [:split {:direction :x :splits [100]}
       [:circle {:radius 15}]
       [:text {:text (-> note-index inc str)
               :font-size 40
               :valign :center
               :halign :left}]]]]
    [:pattern-grid {:pattern note-pattern}]]])

(defn document [{:as attrs :keys [notes-count]}]
  (into [:document {:output "/tmp/autofocus.pdf"
                    :page-size pdf/remarkable-2-page-size
                    :fonts {:default "fonts/Iosevka-Regular.ttf"}}]
    (concat
      [[index-page attrs]]

      (->> (range notes-count)
        (mapv (fn [note-index]
                [note-page {:note-index note-index}]))))))

(comment
  (render/render-document
    (document {:notes-count 200})))
