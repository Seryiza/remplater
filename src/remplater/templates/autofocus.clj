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
  {:layout-left 220
   :layout-top 105})

(def colors
  {:black (pdf/make-color 0 0 0)
   :gray (pdf/make-color 50 50 50)
   :light-gray (pdf/make-color 100 100 100)})

(def styles
  {:layout-border {:width 1
                   :color (:light-gray colors)}
   :week-days-completion-mark {:padding 10}})

(def page-names
  {:day #(str "day-" %)
   :weeks #(str "weeks-" %)
   :weeks-inbox #(str "weeks-inbox-" %)
   :project #(str "project-" %1 "-" %2)})

(def line-height 52)
(def days-per-weeks-page 28)
(def week-days-completion-mark-size 60)
(def week-projects-completion-mark-size 40)
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
   :height line-height
   :outline (fn [{:as attrs :keys [side]} & children]
              (when (#{:bottom :top} side)
                light-gray-line))
   :line light-gray-line
   :row (fn [{:as attrs :keys [row-content row-index]}]
          (when (and row-content row-index)
            (when-let [content (get row-content row-index)]
              content)))})

(defn leftline [{:keys [text target-page]}]
  [:padding {:padding-right 10}
   [:text {:text text
           :font-size 20
           :color (:black colors)
           :halign :right
           :valign :center}
    (when target-page
      [:padding {:padding -10}
       [:page-link {:target-page target-page}]])]])

(defn completion-mark [{:as attrs :keys [x1 y1 x2 y2]}]
  (let [attrs (merge attrs light-gray-line)
        {:keys [x y]} (pos/rect->center attrs)]
    [:div
     [:line (assoc attrs :x1 x1 :y1 y :x2 x :y2 y2)]
     [:line (assoc attrs :x1 x :y1 y2 :x2 x2 :y2 y)]
     [:line (assoc attrs :x1 x2 :y1 y :x2 x :y2 y1)]
     [:line (assoc attrs :x1 x :y1 y1 :x2 x1 :y2 y)]]))

(def page-completion-mark
  [:padding {:padding-top "25%"
             :padding-left 150}
   [:split {:direction :x :splits [day-completion-mark-size]}
    [:split {:direction :y :splits [day-completion-mark-size]}
     [completion-mark]]]])

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

(defn weeks-page [{:keys [from-date to-date]}]
  [:page {:name (get-page-name :weeks from-date)}
   [page-layout
    {:top-left
     page-completion-mark

     :top-right
     [:padding {:padding-left 20}
      [:text {:text (str
                      (t/format (t/formatter "dd MMM") from-date)
                      " - "
                      (t/format (t/formatter "dd MMM") to-date))
              :font-size 40
              :valign :center
              :halign :left}]]

     :bottom-right
     [:split {:direction :y :splits [(* 1 line-height)
                                     (* 12 line-height)
                                     (* 6 line-height)
                                     (* 4 line-height)
                                     (* 1 line-height)
                                     (* 10 line-height)]}
      [:grid {:cols 7 :rows 1
              :line light-gray-line
              :outline (fn [attrs & _]
                         [bottom-outline attrs light-gray-line])}
       (fn [{:keys [index]}]
         (let [columns {0 "MON"
                        1 "TUE"
                        2 "WED"
                        3 "THU"
                        4 "FRI"
                        5 "SAT"
                        6 "SUN"}]
           [:text {:text (get columns index)
                   :color (:gray colors)
                   :font-size 20
                   :valign :center
                   :halign :center}]))]

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
                      :color (:black colors)
                      :font-size 30
                      :valign :top
                      :halign :right}]
              [:text {:text (-> (t/format (t/formatter "MMM") date)
                              (str/upper-case))
                      :color (:gray colors)
                      :font-size 20
                      :valign :top
                      :halign :right}]]]

            [:split {:direction :x :splits [week-days-completion-mark-size]}
             [:split {:direction :y :splits [week-days-completion-mark-size]}
              [:padding (:week-days-completion-mark styles)
               [completion-mark]]]]]))]

      [:pattern-grid {:pattern lines-layout-pattern}]

      [:grid {:cols 4 :rows 2
              :line light-gray-line
              :outline (fn [attrs & _]
                         [bottom-outline attrs light-gray-line])}
       (fn [{:keys [index]}]
         [:page-link {:target-page (get-page-name :project from-date index)}
          [:split {:direction :x :splits [60]}
           [:padding {:padding-top (/ line-height 2)
                      :padding-bottom (/ line-height 2)
                      :padding-left 10}
            [completion-mark]]]])]

      [:grid {:cols 28 :rows 1
              :line light-gray-line
              :outline (fn [attrs & _]
                         [bottom-outline attrs light-gray-line])}
       (fn [{:keys [index]}]
         (let [date (t/>> from-date (t/of-days index))]
           [:padding {:padding 0}
            [:text {:text (t/format (t/formatter "dd") date)
                    :font-size 20
                    :color (:black colors)
                    :halign :center
                    :valign :center}]]))]

      [:grid {:cols 28 :rows 10
              :line light-gray-line}]]

     :bottom-left
     [:pattern-grid {:pattern lines-layout-pattern
                     :row-content {1 [leftline {:text "DAYS>"}]
                                   13 [leftline {:text "NOTES>"}]
                                   18 [leftline {:text "INBOX>"
                                                 :target-page (get-page-name :weeks-inbox from-date)}]
                                   19 [leftline {:text "PROJECTS>"}]
                                   23 [leftline {:text "HABITS>"}]}}]}]])

(defn day-page [{:keys [date weeks-date]}]
  [:page {:name (get-page-name :day date)
          :size (PDRectangle. 1404 6000)}
   [page-layout
    {:top-left
     page-completion-mark

     :top-right
     [:page-link {:target-page (get-page-name :weeks weeks-date)}
      [:padding {:padding-left 20}
       [:text {:text (t/format (t/formatter "dd MMM") date)
               :color (:black colors)
               :font-size 40
               :valign :center
               :halign :left}]]]

     :bottom-left
     [:pattern-grid {:pattern lines-layout-pattern
                     :row-content {1 [leftline {:text "INBOX>"
                                                :target-page (get-page-name :weeks-inbox weeks-date)}]}}]

     :bottom-right
     [:pattern-grid {:pattern lines-layout-pattern}]}]])

(defn project-page [{:keys [weeks-date index]}]
  [:page {:name (get-page-name :project weeks-date index)
          :size (PDRectangle. 1404 6000)}
   [page-layout
    {:top-left
     page-completion-mark

     :top-right
     [:page-link {:target-page (get-page-name :weeks weeks-date)}]

     :bottom-left
     [:pattern-grid {:pattern lines-layout-pattern}]

     :bottom-right
     [:pattern-grid {:pattern lines-layout-pattern}]}]])

(defn inbox-page [{:keys [weeks-date]}]
  [:page {:name (get-page-name :weeks-inbox weeks-date)
          :size (PDRectangle. 1404 6000)}
   [page-layout
    {:top-left
     page-completion-mark

     :top-right
     [:page-link {:target-page (get-page-name :weeks weeks-date)}
      [:padding {:padding-left 10}
       [:text {:text (str
                       (str/upper-case (t/format (t/formatter "MMMM") weeks-date))
                       " INBOX")
               :font-size 40
               :color (:black colors)
               :halign :left
               :valign :center}]]]

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
                [weeks-page {:from-date from-date
                             :to-date (t/>> from-date (t/of-days (dec days-per-weeks-page)))}])))

      (->> (dt/range-dates from-date to-date (t/of-days days-per-weeks-page))
        (mapcat (fn [from-date]
                  (let [weeks-to-date (t/>> from-date (t/of-days days-per-weeks-page))]
                    (->> (dt/range-dates from-date weeks-to-date)
                      (mapv (fn [date]
                              [day-page {:date date
                                         :weeks-date from-date}])))))))

      (->> (dt/range-dates from-date to-date (t/of-days days-per-weeks-page))
        (mapv (fn [from-date]
                [inbox-page {:weeks-date from-date}])))

      (->> (dt/range-dates from-date to-date (t/of-days days-per-weeks-page))
        (mapcat (fn [from-date]
                  (->> (range 8)
                    (mapv (fn [index]
                            [project-page {:weeks-date from-date
                                           :index index}])))))))))

(comment
  (render/render-document
    (document {:from-date (t/new-date 2024 12 9)
               :to-date (t/new-date 2025 1 5)})))
