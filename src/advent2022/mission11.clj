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
  ;; (map println (parse-raw-monkey  test-monkey-raw))

  (let [monkey-lines test-monkey-raw

        [id-line
         items-line
         operation-line
         test-line
         test-line-true
         test-line-false] monkey-lines
        [_ id] (re-matches #"Monkey (.*):" id-line)
        [_ items] (re-matches #"\s*Starting items: (.*)" items-line)
        [_ op arg] (re-matches #"\s*Operation: new = old (.*) (.*)" operation-line)
        [_ div] (re-matches #"\s*Test: divisible by (.*)" test-line)
        [_ true-target] (re-matches #"\s*If .*: throw to monkey (.*)" test-line-true)
        [_ false-target] (re-matches #"\s*If .*: throw to monkey (.*)" test-line-false)
        
        ]

    {:id (utils/as-integer id)
     :items (vec (utils/parse-int-list items))
     :divisable (utils/as-integer div)
     :true-target (utils/as-integer true-target)
     :false-target (utils/as-integer false-target)
     :op {(keyword op) (utils/as-integer arg)}}))


