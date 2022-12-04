(ns advent2022.mission2
  (:require [clojure.java.io :as io]
            [utils.utils :as utils])
  (:gen-class))


(defn parse-input
  "Self : X for Rock, Y for Paper, and Z for Scissors
   Self : A for Rock, B for Paper, and C for Scissors"
  [line]
  (let [[opp _ self] line]
    {:self ({\A :rock
             \B :paper
             \C :scissors } self)
     :opp ({\X :rock
            \Y :paper
            \Z :scissors } opp)}))


(def matches)
(map parse-input (utils/get-lines  "resources/2_input.txt"))

(def rules {{:scissors :rock}       :loose
            {:scissors :paper}      :win
            {:scissors :scissors}   :tie
            {:paper :rock}          :win
            {:paper :paper}         :tie
            {:paper :scissors}      :loose
            {:rock :rock}           :tie
            {:rock :paper}          :loose
            {:rock :scissors}       :win})

(let [round (first matches)
      self (:self round)
      opp (:opp round)
      result (-> { self opp } rules)]
  (println "Self: " self ", Opp: " opp ", Result: " result)
  result)
