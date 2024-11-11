(ns remplater.templates.remarkable-calendar-test
  (:require
    [clojure.test :refer [deftest is]]
    [remplater.render :as render]
    [remplater.templates.remarkable-calendar :as template]
    [tick.core :as t]))

(deftest render-without-exceptions
  (is (nil? (render/render-document
              (template/document
                {:from-date (t/new-date 2024 1 1)
                 :to-date (t/new-date 2025 1 31)})))))
