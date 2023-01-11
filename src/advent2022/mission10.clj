(ns advent2022.mission10
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))


;; Note: noop becomes 'nil' 


(let [initial-cpu {:x 1
                   :cycle 0
                   :op nil }
      instructions (let [parse-line (fn[line]
                                      (let [[_ op v] (re-matches #"(.*) (\d*)" line)]
                                        (cond v {:addx (utils/as-integer v) :op-cycles 4 })))]
                     (map parse-line
                          (utils/get-lines "resources/input_10.txt")
                          ))
      addx-fn (fn[cpu] (let [op (:op cpu)
                             op-cycle-count (:op-cycles op)]
                         (loop [op op]
                           (if (= (:op-cycles op) 0)
                             (-> cpu
                                 (update :x #(+ % (:addx op))) ; effect of op 
                                 (update :cycle #(+ % op-cycle-count)) ; Increase clock cycles of cpu
                                 (dissoc :op)) ; Remove op (it's done)
                             (recur (update op :op-cycles dec))))))

      ]
  
  (loop [cpu       initial-cpu
         instr-seq instructions]

    (let [ins (first instr-seq)
          cpu-fn (fn[cpu
                     ins]
                   (cond (:addx ins) (addx-fn (assoc cpu :op ins)) ; set (assoc) :op in cpu and execute operation, give new cpustate
                         :else                (update cpu :cycle inc))) ; noop
          ] 
      (if (empty? instr-seq) 
        cpu
        (recur (cpu-fn cpu ins) (rest instr-seq))))))


(let [cpu {:x 1
           :cycle 0
           :op {:addx 23
                :op-cycles 0}}

      op-cycle-count 4
      op (:op cpu)]
  (loop [op op]
    (if (= (:op-cycles op) op-cycle-count)
      (-> cpu
          (update :x #(+ % (:addx op))) ; effect of op 
          (update :cycle #(+ % op-cycle-count)) ; Increase clock cycles of cpu
          (dissoc :op)) ; Remove op (it's done)
      (recur (update op :op-cycles inc)))))


(let [cpu {:x 1
           :cycle 0
           :op {:addx 23
                :op-cycles 4}}
      op (:op cpu)
      op-cycle-count (:op-cycles op)]
  (loop [op op]
    (if (= (:op-cycles op) 0)
      (-> cpu
          (update :x #(+ % (:addx op))) ; effect of op 
          (update :cycle #(+ % op-cycle-count)) ; Increase clock cycles of cpu
          (dissoc :op)) ; Remove op (it's done)
      (recur (update op :op-cycles dec)))))

;; signal Strength = the cycle number multiplied by the value of the X register.

;; Check signal-strength every 20nth cycle:  20th, 60th, 100th, 140th, 180th, and 220


(comment "
The CPU has a single register, X, which starts with the value 1. It supports only two instructions:

addx V takes two cycles to complete. After two cycles, the X register is increased by the value V. (V can be negative.)
noop takes one cycle to complete. It has no other effect.
The CPU uses these instructions in a program (your puzzle input) to, somehow, tell the screen what to draw.

Consider the following small program:

noop
addx 3
addx -5
")