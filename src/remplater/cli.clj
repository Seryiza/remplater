(ns remplater.cli
  (:require
    [clojure.java.io :as io]
    [docopt.core :as docopt]
    [remplater.render :as r]
    [remplater.templates.remarkable-calendar :as template-remarkable-calendar]
    [tick.core :as t])
  (:gen-class))

(def usage
  "Remplater: Remarkable Templater.

Usage:
  remplater generate remarkable-calendar [options] --start-date=<2024-01-01> --end-date=<2025-01-31> [--timeline-labels=<0:12,10:18>]
  remplater --help
  remplater --version

Options:
  --help                  Show this screen
  --version               Show version
  --filename=<filename>   Output filename")

(defn some-key [arg-map & arg-keys]
  (some #(when (contains? arg-map %) %) arg-keys))

(defn parse-date [text]
  (t/parse-date text (t/formatter "yyyy-MM-dd")))

(defn generate-template [arg-map]
  (let [template-name (some-key arg-map "remarkable-calendar")
        custom-filename (get arg-map "--filename")
        document (case template-name
                   "remarkable-calendar" (template-remarkable-calendar/document
                                           {:from-date (parse-date (get arg-map "--start-date"))
                                            :to-date (parse-date (get arg-map "--end-date"))}))]
    (cond-> document
      custom-filename (assoc-in [1 :output] custom-filename)
      :always (r/render-document))))

(defn -main [& args]
  (docopt/docopt usage args
    (fn [arg-map]
      (cond
        (some-key arg-map "generate")
        (generate-template arg-map)))))

(comment
  (-main "generate" "remarkable-calendar" "--filename" "/tmp/re2.pdf" "--start-date" "2024-01-01" "--end-date" "2025-01-31"))
