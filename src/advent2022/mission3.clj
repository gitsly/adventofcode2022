(ns advent2022.mission3
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.set :as set])
  (:gen-class))


(defn parse-line
  "returns a rucksack data structure from the input line"
  [line]
  (let [[comp1 comp2] (split-at (/ (count line) 2) line)]
    {:line line
     :comp1 comp1 ; first compartment in the rucksack
     :comp2 comp2
     :common (first (set/intersection (set comp1) (set comp2)))
     }))

;;Lowercase item types a through z have priorities 1 through 26.
;;Uppercase item types A through Z have priorities 27 through 52.
(defn prio
  [in]
  (if (>= (int in) (int \a))
    (inc  (- (int in) (int \a)))
    (+ 27 (- (int in) (int \A)))))

(->> (utils/get-lines "resources/3_input.txt")
     (map parse-line)
     (map :common)
     (map prio)
     (reduce +))


;; Part2 
(defn get-group-info
  [group]
  (let [badge (->> group
                   (map :line)
                   (map set)
                   (reduce set/intersection)
                   first)]
    {:group group
     :badge badge
     :prio (prio badge)
     }))

(->> (utils/get-lines "resources/3_input_full.txt")
     (map parse-line)
     (partition 3) ; 3 sized collections of structure returned by parse-lines
     (map get-group-info)
     (map :prio)
     (reduce +))

