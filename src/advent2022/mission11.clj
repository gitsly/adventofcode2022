(ns advent2022.mission11
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))



(let [
      
      test-monkey-raw (first
                       (partition-by #(= % "")
                                     (utils/get-lines "resources/input_11.txt")))

      parse-raw-monkey (fn[raw-monkey]
                         raw-monkey)

      ]
  (map println (parse-raw-monkey  test-monkey-raw))

  {:items (let [[_ wps] (re-matches #"Starting items: (.*)" "Starting items: 79, 98")]
            (utils/parse-int-list wps))}))
