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

(reduce (fn[a
            b]
          (println a b)

          (vadd 
           a
           b))

        [[1 0]
         [0 2]
         [1 2]])


;; maybe use reduce? 
(defn apply-with-prev
"applies fn taking two parameters with prev item in coll for every item in coll"
[fn
 coll]
(loop [c coll
       prev nil
       res []]
  (if (empty? c)
    res
    (recur (rest c)
           (first c)
           (conj res (fn (first c) prev))))))



(defn exp [x n]
  (reduce * (repeat n x)))

(def motions {:U [0 -1]
              :D [0  1]
              :R [1  0]
              :L [-1 0]})


(defn move
  [H ; 2d vec (head)
   T ; 2D vec (tail)
   v]
  (let [diag-len (vlen [1 1]) ; Length of a diagonal
        H (vadd H v); update H with vector
        ]
    (cond
      (> (vlen (vsub H T)) diag-len) (let [T-next (vadd T v)]
                                       (cond
                                         (> (abs (v 0)) 0) [(T-next 0) (H 1)] ; horiz move (ensure same y)
                                         ;; else: vertical move (ensure same x)
                                         :else [(H 0) (T-next 1)]))
      :else T; No need to move Tail.
      ))) 


(move [2 -1] [1 0] [0 -1]) ; Hmm, if moving, tail seems to always end up in previous Head position.


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
