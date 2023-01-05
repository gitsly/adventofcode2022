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


(let [H [0, 0]
      T [0, 0]

      tail-fn (fn
                [H T]
                (let [len (vlen (vsub H T))]
                  (cond
                    (> len diag-len) {:len len}
                    :else T)))

      move :D
      moves {:U [0 -1]
             :D [0  1]
             :R [1  0]
             :L [-1 0]}]

  (let [H-next (vadd H (move moves))
        diff (vsub H-next T)] 
    {:H H-next
     :T (tail-fn H-next T) }))  


;; Solution
(let [lines (utils/get-lines "resources/9_input.txt")
      parse-line (fn
                   [line]
                   (let [[_ cmd cnt] (re-matches #"(.*) (\d*)" line)]
                     (repeat  (utils/as-integer cnt) (keyword cmd))))

      moves (flatten (map parse-line lines))]

  moves)

