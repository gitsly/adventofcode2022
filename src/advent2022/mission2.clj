(ns advent2022.mission2
  (:require [clojure.java.io :as io]
            [utils.utils :as utils])
  (:gen-class))

(defn get-elf-max-calories[col]
  (->> col
       (partition-by nil?)
       (remove #(= % '(nil)))
       (map #(reduce + %))
       sort
       last))

(defn parse-input
  "Self : X for Rock, Y for Paper, and Z for Scissors
   Self : A for Rock, B for Paper, and C for Scissors"
  [line]
  line)


(let [d "A Z"
      [self _ opp] d]
  {:self ({\A :rock
           \B :paper
           \C :scissors } self)
   :opp ({\X :rock
          \Y :paper
          \Z :scissors } opp)})



(map parse-input (utils/get-lines  "resources/2_input.txt"))
