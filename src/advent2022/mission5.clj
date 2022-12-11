(ns advent2022.mission5
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class))


(defn parse-crate-line-step1
  ([data] (parse-crate-line-step1 data nil))
  ([data acc-data]
   (if (empty? data)
     acc-data
     (recur (drop 4 data)
            (cons (let [[_ crate _] (take 3 data)]
                    crate)
                  acc-data)))))

(defn parse-crate-line
  [data]
  (->> data
       parse-crate-line-step1
       reverse))


(defn parse-move-line
  [input]
  (let [matches (re-matches #"move (\d+) from (\d+) to (\d+)", input)
        [_ cnt from to] matches]
    {:cnt (utils/as-integer cnt)
     :from (utils/as-integer from)
     :to (utils/as-integer to) }))

(defn prepare-crate-data
  "Creates stack (list) for each stack of boxes (from a set of crate data lines)"
  [crates]
  ;;  
  (for [x (range (count (first crates)))]
    (map #(nth % x) crates)
    ))

(defn parse-data
  "Split up input lines different categories and returns crate and
  move data in structured map"
  [file]
  (let [all-lines (utils/get-lines file)
        crate-lines (take-while #(not (string/includes? % "1")) all-lines)
        move-lines (drop (+ 2 (count crate-lines)) all-lines)
        filter-space (fn[coll] (filter #(not (= \space %)) coll))]
    {:crate-data (->> crate-lines
                      (map parse-crate-line)
                      prepare-crate-data
                      (map filter-space))
     :moves (map parse-move-line move-lines)}))

(defn move
  [start-state]
  (loop [state start-state]
    (let [moves (:moves state)
          m (first moves)
          crate-data (:crate-data state)
          from (:from m)
          to  (:to m)
          item (first ((vec crate-data) (dec from)))]
      (if (= 0 (:cnt m))
        (assoc-in state [:moves] (rest moves)) ; move row done
        (do
          (println "Moving" item "from:" from "to" to)
          (recur 
           {:crate-data (for [x (range (count crate-data))]
                          (if (= x (dec from))
                            (drop 1 ((vec crate-data) x))
                            (if (= x (dec to))
                              (cons item ((vec crate-data) x))
                              ((vec crate-data) x))))

            :moves (cons (update-in m [:cnt] dec) (rest moves))}))))))

(defn move-9001
  "Perform move function using modern CrateMover 9001"
  [state]
  (let [moves (:moves state)
        m (first moves)
        cnt (:cnt m)
        crate-data (:crate-data state)
        from (:from m)
        to  (:to m)
        items (take cnt ((vec crate-data) (dec from)))]
    (println "Moving" items "from:" from "to" to)
    {:crate-data (for [x (range (count crate-data))]
                   (if (= x (dec from))

                     (drop cnt ((vec crate-data) x))

                     (if (= x (dec to))
                       (flatten (cons items ((vec crate-data) x)))
                       ((vec crate-data) x))))
     :moves (rest moves)}))

(defn perform-moves
  "Performs all moves using fn-move"
  [fn-move
   start-state]
  (loop [state start-state]
    ;;    (println state)
    (if (empty? (:moves state)) 
      state ; final state
      (recur (fn-move state)))))

;;Part 1
(->> (parse-data "resources/5_input.txt")
     (perform-moves move)
     :crate-data
     (map first)
     (apply str)
     print)
;; -> LBLVVTVLP

;; Part 2
(->> (parse-data "resources/5_input_full.txt")
     (perform-moves move-9001)
     :crate-data
     (map first)
     (apply str)
     print)

