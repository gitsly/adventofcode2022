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

(defn vcap
  "Cap vector at max 1 or min -1"
  [v]
  (vec
   (map #(cond
           (> % 0) (min 1 %)
           :else (max -1 %)) v)))

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



(let [head [2 0]
      tail [1 0]

      move (fn move
             [H T] ;2x 2D vectors
             (let [tail-fn (fn
                             ;; head, tail, move-vector
                             [H T v]
                             (let [diag-len (vlen [1 1])]
                               (cond
                                 (> (vlen (vsub H T)) diag-len) (let [T-next (vadd T v)]
                                                                  (cond
                                                                    (> (abs (v 0)) 0) [(T-next 0) (H 1)] ; horiz move (ensure same y)
                                                                    ;; else: vertical move (ensure same x)
                                                                    :else [(H 0) (T-next 1)]) 
                                                                  )
                                 :else T)))]

               (let [v (vsub T H); vector of move direction for tail to follow H.
                     T-next (tail-fn (vadd H v) T v)] 
                 T-next
                 )))
      ]
  (move head tail))

T.
.*
.H
;; Get next tail pos (in relation to H)
(let [T [0 0]
      H [1 2]

      v (vcap (vsub H T))
      diag-len (vlen [1 1])
      T-next (vadd T v)]
  (println T "+" v "-> " (vadd T v))
  (if (> (vlen (vsub H T)) diag-len)
    (do
      (println "Follow")
      (if (> (abs (v 0)) 0)
        (do
          (println "a")
          [(T-next 0) (H 1)])
        (do
          (println "b")
          [(H 0) (T-next 1)]))) 
    (do
      (println "done")
      T)))

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
