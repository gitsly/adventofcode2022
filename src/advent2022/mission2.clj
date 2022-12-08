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
     :need ({\X :lose
             \Y :draw
             \Z :win } self)}
    ))
;; X means you need to lose,
;; Y means you need to end the round in a draw, and
;; Z means you need to win.



(def rules {
            ;; slef     Opponent
            {:scissors :rock}       :lose
            {:scissors :paper}      :win
            {:scissors :scissors}   :draw
            {:paper :rock}          :win
            {:paper :paper}         :draw
            {:paper :scissors}      :lose
            {:rock :rock}           :draw
            {:rock :paper}          :lose
            {:rock :scissors}       :win})

;; TODO: figure this one out programmatically
;; { need, opponent -> what to select}
(def rules-inv {{:lose :rock}       :scissors
                {:win  :paper}      :scissors
                {:draw :scissors}   :scissors
                {:win  :rock}       :paper
                {:draw :paper}      :paper
                {:lose :scissors}   :paper
                {:draw :rock}       :rock
                {:lose :paper}      :rock
                {:win  :scissors}   :rock})

(def score {:lose 0
            :draw 3
            :win 6
            :rock 1
            :paper 2
            :scissors 3 })

(defn target-self
  "Used when self choice needs to target :need"
  [round]
  (let [need (:need round)
        opp (:opp round)]
    (rules-inv { need opp })))



;; Goal: reverse the rules map, by making a submap for each :need with
;; opponents choice, and what needs to be done by self to meet :need
;;(def rules {'a :lose
;;            'b :lose
;;            'c :win
;;            'd :win})
;;(reduce (fn [sum, item] (merge sum item))
;;        (map vector (vals rules) (keys rules)))

(defn resolve-round
  [self-fn ; function that takes rount as argument, and returns what to select against opponent
   round]
  (let [
        self (self-fn round)
        opp (:opp round)
        result (-> { self opp } rules)]
    (println "Self: " self ", Opp: " opp ", Result: " result)
    {:result result
     :score (+ (score result)
               (score self))}))

;;(resolve-round :self {:opp :rock, :self :paper, :need :draw})

(->> (map parse-input (utils/get-lines "resources/2_input.txt"))
     (map target-self))

;; Part1
(->> (map parse-input (utils/get-lines "resources/2_input.txt"))
     (map #(resolve-round :self %))
     (map :score)
     (reduce +))

;; Part2
(->> (map parse-input (utils/get-lines "resources/2_input.txt"))
     (map #(resolve-round target-self %))
     (map :score)
     (reduce +))
