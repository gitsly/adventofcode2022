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
  (for [x (range (count crates))]
    (map #(nth % x) crates)
    ))

;; Split up the data in different categories
(defn parse-data
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





;; Testing
(parse-crate-line "    [D]    ")
(parse-crate-line "[Z] [M] [P]")
(parse-crate-line "move 2 from 2 to 1")

(defn move
  "Takes a move and crate state as input and returns new crate state"
  [move crate-data]
  (let [from (dec (:from move))
        to (dec (:to move))
        item (first ((vec crate-data) from))]
    (println "Moving" item "from:" from "to" to)
    (for [x (range (count crate-data))]
      (if (= x from)
        (drop 1 ((vec crate-data) x))
        (if (= x to)
          (cons item ((vec crate-data) x))
          ((vec crate-data) x))))))

(let [data (parse-data "resources/5_input.txt")
      moves (:moves data)
      crate-data (:crate-data data)]
  (println "crate-data" crate-data)
  (move (first moves) crate-data))
