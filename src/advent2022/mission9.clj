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



;; Demo partition
(let [input [1 2 3 4]
      step 1
      size 2]
  (partition size step input))

;; reduce with initial 'val'
(let [input [3 4]
      step 1
      size 2
      val 2]
  (reduce (fn add
            [a b]
            (println a "+" b \= (+ a b))
            (+ a b)) val input))


(defn exp [x n]
  (reduce * (repeat n x)))


(def motions {:U [0 -1]
              :D [0  1]
              :R [1  0]
              :L [-1 0]})


;; Hmm, if moving, tail seems to always end up in previous Head position.
(defn move
  [new ; new head pos
   old ; old head pos (before applying vector for a move)
   tail]
  (let [diag-len (vlen [1 1])]
    (if (> (vlen (vsub new tail)) diag-len) 
      old
      tail))) 

(let [head [2 -1]
      tail [1 0]
      dir [0 -1]
      tail-new (move (vadd head dir) head tail)
      tail-move-vec (vsub 
                     tail-new 
                     tail)]
  tail-move-vec)

(let [head [1 0]
      tail [0 0]
      dir [0 -1]]
  (move (vadd head dir) head tail))


;; Initial
....
..H.
21..

;; 1 neeed to follow H
..H.
....
21..

;; ?NEW? head movement (diagonal), 1 being the head., This doesnt allow for the T taking old H's position whenever needing to move rule..
..H.
..1.
2...

;; 2 will follow the 1 
..H.
.21.
....

;; Above, the 2 is following the 1 in a new 'motion'


;; Solution
(let [lines (utils/get-lines "resources/9_input_full.txt")
      parse-line (fn
                   [line]
                   (let [[_ cmd cnt] (re-matches #"(.*) (\d*)" line)]
                     (repeat  (utils/as-integer cnt) (keyword cmd))))

      all-moves (map motions (flatten (map parse-line lines)))

      all-moves (map motions [:R])

      move-all (fn [state
                    m]
                 (update state :knots 
                         #(apply-with-prev (fn[tail
                                               head]
                                             (println "Head:" head "Tail:" tail)
                                             (if (nil? head)
                                               :head
                                               ((move head tail m) 1))

                                             )

                                           %))

                 )

      knot-count 2

      final-state (loop [state {:knots (repeat knot-count [0 0])
                                :T-history #{[0 0]} }
                         moves all-moves]
                    (if (nil? (first moves))
                      state
                      (recur
                       (move-all state (first moves))
                       (rest moves))))

      tail-visited-positions (count
                              (:T-history final-state))
      ]

  all-moves )
