(ns advent2022.mission3
  (:require [clojure.java.io :as io]
            [utils.utils :as utils])
  (:gen-class))

(def rucksack {:comparments
               {:big1 nil
                :big2 nil}})

(def bag1 "vJrwpWtwJgWrhcsFMMfFFhFp")

(defn parse-line
  "returns a bag data structure from the input line"
  [line]
  (let [[comp1 comp2] (split-at (/ (count line) 2) line)]
    { :comp1 comp1
     :comp2 comp2
     }))

(parse-line bag1)

;; Every item type is identified by a single lowercase or uppercase
;; letter (that is, a and A refer to different types of items).
