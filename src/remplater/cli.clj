(ns remplater.cli
  (:require
    [docopt.core :as docopt])
  (:gen-class))

(def usage
  "Remplater: Remarkable Templater.

Usage:
  remplater templates remarkable-calendar [--start-date=<start-date-argument>]")

(defn -main [& args]
  (docopt/docopt usage args
    (fn [arg-map]
      (prn "!" arg-map))))
