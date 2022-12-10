(ns advent2022.mission5
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class))


(defn parse-crate-line-step1
  ([data] (parse-crate-line-step1 data nil))
  ([data acc-data]
   (println data)
   (if (empty? data)
     acc-data
     (recur (drop 4 data)
            (cons (let [[_ crate _] (take 3 data)]
                    crate)
                  acc-data)))))

(defn parse-crate-line
  [data]
  (->> data
       parse-crate-line-step1
       reverse))

;; (parse-crate-line "    [D]    ")
(parse-crate-line "[Z] [M] [P]")


;; Split up the data in different categories
(let [all-lines (utils/get-lines "resources/5_input.txt")
      crate-lines (take-while #(not (string/includes? % "1")) all-lines)
      moves (drop (+ 2 (count crate-lines)) all-lines)]
  {:crate-lines crate-lines
   :moves moves})

(string/includes?  "input""i" )



(let [all-lines (utils/get-lines "resources/5_input.txt")]
  (take (dec (count all-lines)) all-lines))


