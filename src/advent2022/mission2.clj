(ns advent2022.mission2
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.set :as set])
  (:gen-class))


(defn parse-input
  "Self : X for Rock, Y for Paper, and Z for Scissors
   Self : A for Rock, B for Paper, and C for Scissors"
  [line]
  (let [[opp _ self] line]
    {:opp ({\A :rock
            \B :paper
            \C :scissors } opp)
     :self ({\X :rock
             \Y :paper
             \Z :scissors } self)
     :need ({\X :loose
             \Y :draw
             \Z :win } self)}
    ))
;; X means you need to lose,
;; Y means you need to end the round in a draw, and
;; Z means you need to win.



(def rules {{:scissors :rock}       :loose
            {:scissors :paper}      :win
            {:scissors :scissors}   :tie
            {:paper :rock}          :win
            {:paper :paper}         :tie
            {:paper :scissors}      :loose
            {:rock :rock}           :tie
            {:rock :paper}          :loose
            {:rock :scissors}       :win})

(def score {:loose 0
            :tie 3
            :win 6
            :rock 1
            :paper 2
            :scissors 3 })

(set/map-invert rules)

(def need-to {})

(defn resolve-round
  [round]
  (let [self (:self round)
        opp (:opp round)
        result (-> { self opp } rules)]
    (println "Self: " self ", Opp: " opp ", Result: " result)
    {:result result
     :score (+ (score result)
               ;; Add score for 'shape' only if winning
               (score self))}))


(:need (parse-input "A Y"))
(:need (parse-input "B X"))
(:need (parse-input "C Z"))

;; This is the solution
(->> (map parse-input (utils/get-lines "resources/2_input.txt"))
     (map resolve-round)
     (map :score)
     (reduce +))
