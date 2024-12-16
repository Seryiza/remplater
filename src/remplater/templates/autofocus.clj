(ns remplater.templates.autofocus
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.positioning :as pos]
    [remplater.components.render :as render]
    [remplater.datetime :as dt]
    [remplater.pdf :as pdf]
    [tick.core :as t])
  (:import
    [org.apache.pdfbox.pdmodel.common PDRectangle]))

(def paddings
  {:layout-left 200
   :layout-top 105})

(def colors
  {:gray (pdf/make-color 50 50 50)
   :light-gray (pdf/make-color 100 100 100)})

(def styles
  {:layout-border {:width 1
                   :color (:light-gray colors)}
   :week-days-completion-mark {:padding 10}})

(def page-names
  {:day #(str "day-" %)
   :weeks #(str "weeks-" %)})

(def days-per-weeks-page 28)
(def week-days-completion-mark-size 60)
(def day-completion-mark-size 60)

(defn get-page-name [page-name & params]
  (apply (get page-names page-name) params))

(def light-gray-line
  [:line {:width 1
          :color (:light-gray colors)}])

(defn bottom-outline [{:as attrs :keys [side]} & children]
  (when (= :bottom side)
    children))

(def lines-layout-pattern
  {:width "100%"
   :height 52
   :outline (fn [{:as attrs :keys [side]} & children]
              (when (#{:bottom :top} side)
                light-gray-line))
   :line light-gray-line})

(defn completion-mark [{:as attrs :keys [x1 y1 x2 y2]}]
  (let [attrs (merge attrs light-gray-line)
        {:keys [x y]} (pos/rect->center attrs)]
    [:div
     [:line (assoc attrs :x1 x1 :y1 y :x2 x :y2 y2)]
     [:line (assoc attrs :x1 x :y1 y2 :x2 x2 :y2 y)]
     [:line (assoc attrs :x1 x2 :y1 y :x2 x :y2 y1)]
     [:line (assoc attrs :x1 x :y1 y1 :x2 x1 :y2 y)]]))

(defn page-layout [{:keys [top-left top-right bottom-left bottom-right]}]
  [:split {:direction :y :splits [(:layout-top paddings)]}
   [:div
    [:border {:border-bottom (:layout-border styles)}]
    [:split {:direction :x :splits [(:layout-left paddings)]}
     [:div
      [:border {:border-right (:layout-border styles)}]
      top-left]
     top-right]]

   [:split {:direction :x :splits [(:layout-left paddings)]}
    [:div
     [:border {:border-right (:layout-border styles)}]
     bottom-left]
    bottom-right]])

(defn weeks-page [{:keys [from-date]}]
  [:page {:name (get-page-name :weeks from-date)}
   [page-layout
    {:bottom-right
     [:split {:direction :y :splits [700]}
      [:grid {:cols 7 :rows 4
              :line light-gray-line
              :outline (fn [attrs & _]
                         [bottom-outline attrs light-gray-line])}
       (fn [{:keys [index]}]
         (let [date (t/>> from-date (t/of-days index))]
           [:page-link {:target-page (get-page-name :day date)}
            [:padding {:padding 10}
             [:split {:direction :y :splits [30]}
              [:text {:text (t/format (t/formatter "dd") date)
                      :color (:gray colors)
                      :font-size 30
                      :valign :top
                      :halign :right}]
              [:text {:text (-> (t/format (t/formatter "MMM") date)
                              (str/upper-case))
                      :color (:light-gray colors)
                      :font-size 20
                      :valign :top
                      :halign :right}]]]

            [:split {:direction :x :splits [week-days-completion-mark-size]}
             [:split {:direction :y :splits [week-days-completion-mark-size]}
              [:padding (:week-days-completion-mark styles)
               [completion-mark]]]]]))]]}]])

(defn day-page [{:keys [date weeks-date]}]
  [:page {:name (get-page-name :day date)
          :size (PDRectangle. 1404 6000)}
   [page-layout
    {:top-left
     [:padding {:padding-top "25%"
                :padding-left 120}
      [:split {:direction :x :splits [day-completion-mark-size]}
       [:split {:direction :y :splits [day-completion-mark-size]}
        [completion-mark]]]]

     :top-right
     [:page-link {:target-page (get-page-name :weeks weeks-date)}
      [:padding {:padding-left 20}
       [:text {:text (t/format (t/formatter "dd MMM") date)
               :font-size 40
               :valign :center
               :halign :left}]]]

     :bottom-left
     [:pattern-grid {:pattern lines-layout-pattern}]

     :bottom-right
     [:pattern-grid {:pattern lines-layout-pattern}]}]])

(defn document [{:as attrs :keys [from-date to-date]}]
  (into [:document {:output "/tmp/autofocus.pdf"
                    :page-size pdf/remarkable-2-page-size
                    :fonts {:default "fonts/Iosevka-Regular.ttf"}}]
    (concat
      (->> (dt/range-dates from-date to-date (t/of-days days-per-weeks-page))
        (mapv (fn [from-date]
                [weeks-page {:from-date from-date}])))

      (->> (dt/range-dates from-date to-date (t/of-days days-per-weeks-page))
        (mapcat (fn [from-date]
                  (let [weeks-to-date (t/>> from-date (t/of-days days-per-weeks-page))]
                    (->> (dt/range-dates from-date weeks-to-date)
                      (mapv (fn [date]
                              [day-page {:date date
                                         :weeks-date from-date}]))))))))))

(comment
  (render/render-document
    (document {:from-date (t/new-date 2024 12 9)
               :to-date (t/new-date 2025 1 5)})))
