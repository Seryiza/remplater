(ns remplater.templates.alpha
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.render :as render]
    [remplater.datetime :as dt]
    [remplater.pdf :as pdf]
    [tick.core :as t]))

(defn get-month-page-name [date]
  (str "month-page-" (t/format dt/fmt-yyyy date) "-" (t/format dt/fmt-mm date)))

(defn get-month-inbox-page-name [date]
  (str "month-inbox-page-" (t/format dt/fmt-yyyy date) "-" (t/format dt/fmt-mm date)))

(defn get-day-page-name [date]
  (str "day-page-" (t/format dt/fmt-yyyy date) "-" (t/format dt/fmt-mm date) "-" (t/format dt/fmt-dd date)))

(defn get-notes-index-page-name [page-index]
  (str "notes-index-page-" page-index))

(defn get-notes-index-alias-by-note [note-index]
  (str "notes-index-page-by-note-" note-index))

(defn get-notes-page-name [note-index]
  (str "notes-page-" note-index))

(defn get-monthly-weeks [{:keys [date]}]
  (comment
    (get-monthly-weeks {:date (t/new-date 2024 9 1)}))

  (let [calendar-start (-> (t/first-day-of-month date)
                         (t/previous-or-same t/MONDAY))
        calendar-end (-> (t/last-day-of-month date)
                       (t/next-or-same t/SUNDAY))]
    (->> (dt/range-dates calendar-start calendar-end (t/of-days 7))
      (mapv #(t/format dt/fmt-w %)))))

(defn get-monthly-days [{:keys [date]}]
  (comment
    (get-monthly-days {:date (t/new-date 2024 9 1)})
    (get-monthly-days {:date (t/new-date 2024 10 1)}))

  (let [month-start (t/first-day-of-month date)
        month-end (t/last-day-of-month date)
        calendar-start-day (t/previous-or-same month-start t/MONDAY)
        calendar-end-day (t/next-or-same month-end t/SUNDAY)]
    (->> (dt/range-dates calendar-start-day calendar-end-day)
      (mapv (fn [date]
              {:label (t/format dt/fmt-dd date)
               :page-name (get-day-page-name date)
               :this-month? (= (t/month date) (t/month month-start))
               :weekend? (dt/weekend? date)})))))

(def line-pattern-height 51)
(def double-vertical-line-gap 10)
(def subtitle-font-size 22)
(def sideline-font-size 22)

(defn light-line [{:as attrs :keys [col-index]} & children]
  [:line {:dash {:pattern [3]
                 :phase 1}}])

(defn normal-line [attrs & children]
  [:line {}])

(defn double-vertical-line [attrs]
  [:div
   [:border {:border-right {:width 1}}]
   [:padding {:padding-right double-vertical-line-gap}
    [:border {:border-right {:width 1}}]]])

(defn single-horizontal-line [attrs]
  [:border {:border-bottom {:width 1}}])

(defn sideline-item [attrs & children]
  [:padding {:padding-right 50}
   [:text {:text (:text attrs)
           :font-size sideline-font-size
           :valign :center
           :halign :right}
    (into [:padding {:padding -15}] children)]])

(defn bottom-light-outline [{:as attrs :keys [side]} & children]
  (when (= :bottom side)
    [light-line]))

(defn bottom-normal-outline [{:as attrs :keys [side]} & children]
  (when (= :bottom side)
    [normal-line]))

(def line-pattern
  {:height line-pattern-height
   :row-count 30
   :line light-line
   :outline bottom-normal-outline
   :row (fn [{:as attrs :keys [row-content row-index]}]
          (when (and row-content row-index)
            (when-let [content (get row-content row-index)]
              content)))})

(def timegrid-pattern
  (merge line-pattern
    {:width "25%"}))

(def timeline-pattern
  (merge line-pattern
    {:line (fn [{:keys [row-index rows]}]
             (let [rows-count (count rows)
                   hour? (and (odd? row-index)
                           (<= 1 row-index)
                           (<= row-index (- rows-count 2)))
                   hour (+ 9 (/ (inc row-index) 2))]
               [:div
                [light-line {}]
                (when hour?
                  [:padding {:padding-right 70
                             :padding-bottom 0}
                   [:text {:text (str hour)
                           :font-size 25
                           :valign :center
                           :halign :right}
                    [:rect {:fill {:color (pdf/make-color 255 255 255)}}]]])]))}))

(def month-weeks-pattern
  {:height (* 3 line-pattern-height)
   :line normal-line
   :outline bottom-normal-outline
   :row (fn [{:keys [weeks rows row-index]}]
          (let [week-number (get weeks row-index)]
            [:padding {:padding-right double-vertical-line-gap}
             [:pattern-grid {:pattern line-pattern
                             :row-content {0 [sideline-item {:text (str "WEEK " week-number ">")}]}}]]))})

(def month-days-pattern
  (merge month-weeks-pattern
    {:width 1/7}))

(defn page-layout [attrs & children]
  (let [split-y 250]
    [:div
     [:split {:direction :x :splits [300]}
      [:split {:direction :y :splits [split-y]}
       [:div
        [double-vertical-line {}]
        [single-horizontal-line {}]

        [:padding {:padding-top 90
                   :padding-left 130}
         [:split {:direction :y :splits [100]}
          (:top-left-title attrs)
          (:top-left-subtitle attrs)]]

        (:top-left attrs)]
       [:div
        [double-vertical-line {}]
        (:bottom-left attrs)]]
      [:split {:direction :y :splits [split-y]}
       [:div
        [single-horizontal-line {}]

        [:padding {:padding-top 190}
         (:top-right-subtitles attrs)]

        [:split {:direction :x :splits ["50%"]}
         [:split {:direction :x :splits ["50%"]}
          [:padding {:padding-top 90
                     :padding-left 30}
           [:split {:direction :y :splits [100]}
            (:top-right-title attrs)
            [:padding {:padding-left 5}
             (:top-right-subtitle attrs)]]]
          [:padding {:padding-top 190}
           (:top-right-subtitle-2 attrs)]]
         [:split {:direction :x :splits ["50%"]}
          [:padding {:padding-top 190}
           (:top-right-subtitle-3 attrs)]
          [:padding {:padding-top 190}
           (:top-right-subtitle-4 attrs)]]]

        (:top-right attrs)]
       [:div
        (:bottom-right attrs)]]]]))

(defn day-page [{:as attrs :keys [date]}]
  [:page {:name (get-day-page-name date)}
   [page-layout
    {:top-left-title
     [:text {:text (t/format dt/fmt-dd date)
             :font-size 80
             :halign :center}]

     :top-left-subtitle
     [:page-link {:target-page (get-month-page-name date)}
      [:text {:text (str
                      (str/upper-case
                        (t/format dt/fmt-mmm date))
                      ">")
              :font-size subtitle-font-size
              :halign :center}]]

     :top-right-title
     [:text {:text (str/upper-case
                     (t/format dt/fmt-day-of-week date))
             :font-size 80
             :halign :left}]

     :top-right-subtitle
     [:text {:text (str "WEEK " (t/format dt/fmt-w date) ">")
             :font-size subtitle-font-size
             :halign :left}]

     :top-right-subtitle-3
     [:page-link {:target-page (get-month-inbox-page-name date)}
      [:text {:text "INBOX>"
              :font-size subtitle-font-size
              :halign :left}]]

     :top-right-subtitle-4
     [:text {:text "NOTES>"
             :font-size subtitle-font-size
             :halign :left}]

     :bottom-left
     [:pattern-grid {:pattern timeline-pattern
                     :vertical-align :top}]

     :bottom-right
     [:pattern-grid {:pattern timegrid-pattern
                     :vertical-align :top}]}]])

(defn month-index-page [{:keys [date]}]
  [:page {:name (get-month-inbox-page-name date)}
   [page-layout
    {:top-left-title
     [:text {:text (t/format dt/fmt-mm date)
             :font-size 80
             :halign :center}]

     :top-left-subtitle
     [:page-link {:target-page (get-month-page-name date)}
      [:text {:text (str
                      (str/upper-case
                        (t/format dt/fmt-mmm date))
                      ">")
              :font-size subtitle-font-size
              :halign :center}]]

     :top-right-title
     [:text {:text (str/upper-case
                     (str (t/format dt/fmt-mmmm date) " INBOX"))
             :font-size 80
             :halign :left}]

     :top-right-subtitle
     [:text {:text (str "SEP INB>")
             :font-size subtitle-font-size
             :halign :left}]

     :top-right-subtitle-2
     [:text {:text (str "DEC INB>")
             :font-size subtitle-font-size
             :halign :left}]

     :top-right-subtitle-4
     [:text {:text "NOTES>"
             :font-size subtitle-font-size
             :halign :left}]

     :bottom-left
     [:pattern-grid {:pattern line-pattern
                     :vertical-align :top}]

     :bottom-right
     [:pattern-grid {:pattern line-pattern
                     :vertical-align :top}]}]])

(defn month-page [{:keys [date]}]
  (let [month-weeks (get-monthly-weeks {:date date})
        month-days (get-monthly-days {:date date})
        weeks-count (count month-weeks)]
    [:page {:name (get-month-page-name date)}
     [page-layout
      {:top-left-title
       [:text {:text (t/format dt/fmt-mm date)
               :font-size 80
               :halign :center}]

       :top-left-subtitle
       [:text {:text (str
                       (str/upper-case
                         (t/format dt/fmt-yyyy date))
                       ">")
               :font-size subtitle-font-size
               :halign :center
               :valign :center}]

       :top-right-title
       [:text {:text (str/upper-case
                       (t/format dt/fmt-mmmm date))
               :font-size 80
               :halign :left}]

       :top-right-subtitles
       [:grid {:rows 1 :cols 7}
        (fn [{:keys [index]}]
          [:text {:text (case index
                          0 "MONDAY"
                          1 "TUESDAY"
                          2 "WEDNESDAY"
                          3 "THURSDAY"
                          4 "FRIDAY"
                          5 "SATURDAY"
                          6 "SUNDAY")
                  :font-size subtitle-font-size
                  :valign :center
                  :halign :center}])]

       :bottom-left
       [:aligned-according-to-pattern {:pattern line-pattern}
        [:split {:direction :y :splits [(* 3 weeks-count line-pattern-height)]}
         [:pattern-grid {:pattern month-weeks-pattern
                         :vertical-align :top
                         :weeks month-weeks}]
         [:pattern-grid {:pattern line-pattern
                         :vertical-align :top
                         :row-content {0 [sideline-item {:text "NOTES>"}]
                                       3 [sideline-item {:text "INBOX>"}
                                          [:page-link {:target-page (get-month-inbox-page-name date)}]]}}]]]

       :bottom-right
       [:aligned-according-to-pattern {:pattern line-pattern}
        [:split {:direction :y :splits [(* 3 weeks-count line-pattern-height)]}
         [:grid {:cols 7 :rows weeks-count
                 :line normal-line
                 :outline bottom-normal-outline}
          (fn [{:keys [index]}]
            (let [{:keys [page-name label this-month?]} (get month-days index)]
              [:padding {:padding 15}
               (when this-month?
                 [:page-link {:target-page page-name}])

               [:text {:text (str label
                               (when this-month? ">"))
                       :color (if this-month?
                                (pdf/make-color 0 0 0)
                                (pdf/make-color 150 150 150))
                       :font-size sideline-font-size
                       :halign :right
                       :valign :top}]]))]
         [:pattern-grid {:pattern line-pattern
                         :vertical-align :top}]]]}]]))

(defn document [{:keys [from-date to-date]}]
  (into [:document {:output "/tmp/alpha.pdf"
                    :page-size pdf/remarkable-2-page-size
                    :fonts {:default "fonts/Iosevka-Regular.ttf"
                            :bold "fonts/Iosevka-Bold.ttf"}}]
    (concat
      (->> (dt/range-dates from-date to-date (t/of-months 1))
        (mapv (fn [date]
                [month-page {:date date}])))

      (->> (dt/range-dates from-date to-date (t/of-months 1))
        (mapv (fn [date]
                [month-index-page {:date date}])))

      (->> (dt/range-dates from-date to-date (t/of-days 1))
        (mapv (fn [date]
                [day-page {:date date}]))))))

(comment
  (render/render-document
    (document {:from-date (t/new-date 2024 11 1)
               :to-date (t/new-date 2024 11 30)})))
