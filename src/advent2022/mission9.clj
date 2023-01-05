(ns advent2022.mission9
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))


;; Moves

(defn vsub
  [a b]
  (vec
   (map - a b)))

(defn vadd
  [a b]
  (vec
   (map + a b)))

(defn vlen
  "Calculates length of vector"
  [v]
  (Math/sqrt (+ (exp (v 0) 2)
                (exp (v 1) 2))))

(def diag-len (vlen [1 1])) ; Length of a diagonal


(defn exp [x n]
  (reduce * (repeat n x)))


(let [T [0, 1]
      H [1, 0]
      tail-fn (fn
                [H T move]
                (cond
                  (> (vlen (vsub H T)) diag-len) (let [T-next (vadd T move)]
                                                   (cond
                                                     (> (abs (move 0)) 0) [(T-next 0) (H 1)] ; horiz move (ensure same y)
                                                     :else [(H 0) (T-next 1)]) ; vertical move (ensure same x)
                                                   )
                  :else T)) ; No need to move Tail.

      move-id :R
      moves {:U [0 -1]
             :D [0  1]
             :R [1  0]
             :L [-1 0]}]

  (let [H-next (vadd H (move-id moves))] 
    {:H H-next
     :T (tail-fn H-next T (move-id moves)) }))  


;; Solution
(let [lines (utils/get-lines "resources/9_input.txt")
      parse-line (fn
                   [line]
                   (let [[_ cmd cnt] (re-matches #"(.*) (\d*)" line)]
                     (repeat  (utils/as-integer cnt) (keyword cmd))))

      moves (flatten (map parse-line lines))]

  moves)

