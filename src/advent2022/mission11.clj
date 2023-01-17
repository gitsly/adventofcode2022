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
                            :inspect-count 0 ; How many times a monkey has inspected and thrown items
                            :divisible (utils/as-integer div)
                            :true-target (utils/as-integer true-target)
                            :false-target (utils/as-integer false-target)
                            :op {(keyword op) (utils/as-integer arg)}})
                         )

      monkeys (map parse-raw-monkey
                   (filter #(not (= % '("")))
                           (partition-by #(= % "")
                                         (utils/get-lines "resources/input_11.txt"))))


      turn (fn turn []
             ;; "On a single monkey's turn, it inspects and throws all of the items it is holding one at a time and in the order listed."
             )

      round (fn round
              ;;"The process of each monkey taking a single turn is called a round."
              [state]
              state)

      eval (fn eval
             [state]
             (lazy-seq (cons (round state) (eval state))))

      inspect-item (fn
                     [monkey]
                     (let [item (first (:items monkey))]
                       (println "Monkey inspects an item with a worry level of" item ".")
                       item))
      ]
  (take 20 (eval monkeys))


  (inspect-item (first monkeys))


  )



(comment "
Monkey 0:
  Monkey inspects an item with a worry level of 79.
    Worry level is multiplied by 19 to 1501.
    Monkey gets bored with item. Worry level is divided by 3 to 500.
    Current worry level is not divisible by 23.
    Item with worry level 500 is thrown to monkey 3.
  Monkey inspects an item with a worry level of 98.
    Worry level is multiplied by 19 to 1862.
    Monkey gets bored with item. Worry level is divided by 3 to 620.
    Current worry level is not divisible by 23.
    Item with worry level 620 is thrown to monkey 3.
")

