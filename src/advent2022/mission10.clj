(ns advent2022.mission10
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))


(let [parse-line (fn[line]
                   (let [[_ op v] (re-matches #"(.*) (\d*)" line)]
                     (cond v {:addx (utils/as-integer v)})))]
  (map parse-line
       (utils/get-lines "resources/input_10.txt")
       ))


(let [initial-cpu {:x 1
                   :op-cnt 0}
      ]
  
  (loop [cpu initial-cpu
         instr ]))



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
