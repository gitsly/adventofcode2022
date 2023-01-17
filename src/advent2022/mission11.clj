(ns advent2022.mission11
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))


(let [parse-raw-monkey (fn[raw-monkey]
                         (let [[id-line
                                items-line
                                operation-line
                                test-line
                                test-line-true
                                test-line-false] raw-monkey
                               [_ id] (re-matches #"Monkey (.*):" id-line)
                               [_ items] (re-matches #"\s*Starting items: (.*)" items-line)
                               [_ op arg] (re-matches #"\s*Operation: new = old (.*) (.*)" operation-line)
                               [_ div] (re-matches #"\s*Test: divisible by (.*)" test-line)
                               [_ true-target] (re-matches #"\s*If .*: throw to monkey (.*)" test-line-true)
                               [_ false-target] (re-matches #"\s*If .*: throw to monkey (.*)" test-line-false)]

                           {:id (utils/as-integer id)
                            :items (vec (utils/parse-int-list items))
                            :divisable (utils/as-integer div)
                            :true-target (utils/as-integer true-target)
                            :false-target (utils/as-integer false-target)
                            :op {(keyword op) (utils/as-integer arg)}})
                         )

      monkeys (map parse-raw-monkey
                   (filter #(not (= % '("")))
                           (partition-by #(= % "")
                                         (utils/get-lines "resources/input_11.txt"))))
      ]

  monkeys)


