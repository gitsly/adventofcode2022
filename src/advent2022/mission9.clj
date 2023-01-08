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

(defn apply-with-prev
  "applies fn(prev-res,i) item in coll for every item in coll
  where the prev-res is the result of the prev fn call"
  ([fn
    coll]
   (apply-with-prev fn nil coll))
  ([fn
    init
    coll]
   (loop [c coll
          prev init
          res []]
     (if (empty? c)
       res
       (let [r (fn prev (first c))]
         (recur (rest c)
                r
                (conj res r)))))))

(apply-with-prev (fn[a b](+ a b))
                 0 [0 1 2 3 4 5])

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

(defn tail-move-fn
  [H T]
  (let [v (vcap (vsub H T))
        diag-len (vlen [1 1])
        T-next (vadd T v)
        result (if (> (vlen (vsub H T)) diag-len)
                 (if (> (abs (v 0)) 0)
                   [(T-next 0) (H 1)]
                   [(H 0) (T-next 1)]) 
                 T)]
    (println "Head:" H "Tail:" T "->" result)
    result))



(defn move2)
(let [head [0 0]
      tail [0 0]
      v    [1 0]
      new-head (vadd head v)]
  (cons new-head
        (drop-last (apply-with-prev tail-move-fn new-head [head tail]))))



(tail-move-fn [-2 0] [0 0])

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
