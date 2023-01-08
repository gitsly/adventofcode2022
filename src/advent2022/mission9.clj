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

(defn exp [x n]
  (reduce * (repeat n x)))

(defn vlen
  "Calculates length of vector"
  [v]
  (Math/sqrt (+ (exp (v 0) 2)
                (exp (v 1) 2))))

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


(def motions {:U [0 -1]
              :D [0  1]
              :R [1  0]
              :L [-1 0]})

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
    ;;(println "Head:" H "Tail:" T "->" result)
    result))


(defn move
  [coll
   v]
  (let [head (first coll)
        tail (last coll)
        new-head (vadd head v)]
    (drop-last (cons new-head (apply-with-prev tail-move-fn new-head coll)))))

;;(move [[2 0] [1 0] [0 0]] [1 0])

;; 

(let [knots [[2 -2] [1 -2] [0 -2] [1 -2] [2 -2] [3 -2] [4 -2] [5 -2] [5 -3] [4 -3]]
      min-x  (apply min (map first knots))
      max-x  (apply max (map first knots))
      min-y  (apply min (map second knots))
      max-y  (apply max (map second knots))
      ;;      span-x (- max-x min-x)
      ;;      span-y (- max-y min-y)
      points (zipmap knots
                     (map char (cons \H (range (int \1) (+ (int \0) 9)))))

      xrange (range min-x (inc max-x))
      yrange (reverse (range min-y (inc max-y)))

      toprint (for [y yrange
                    x xrange]
                ;;{[x y]}
                (if (nil? (get points [x y]))
                  \.
                  (get points [x y])))]
  ;; (range min-x (inc max-x))

  (map println  
       (map #(apply str %) (partition (count xrange) toprint)))))




;; Solution
(let [lines (utils/get-lines "resources/9_input.txt")
      parse-line (fn
                   [line]
                   (let [[_ cmd cnt] (re-matches #"(.*) (\d*)" line)]
                     (repeat  (utils/as-integer cnt) (keyword cmd))))

      all-moves (map motions (flatten (map parse-line lines)))

      ;; Test moves
      ;;      all-moves (map motions [:R :R])

      knot-count 10

      move-all (fn [state
                    m]
                 (update state :knots #(move % m)))

      update-history (fn [state]
                       ;;(println "Tail: " (last (:knots state)))
                       (update state :T-history #(conj % (last (:knots state)))))


      final-state (loop [state {:knots (repeat knot-count [0 0])
                                :T-history #{[0 0]} }
                         moves all-moves]
                    (if (nil? (first moves))
                      state
                      (recur
                       (update-history
                        (move-all state (first moves)))
                       (rest moves))))

      tail-visited-positions (count
                              (:T-history final-state))]
;; tail-visited-positions
(:knots final-state)

)


;; PartII 7053 too high
