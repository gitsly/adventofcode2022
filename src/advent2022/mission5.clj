(ns advent2022.mission5
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class))


(defn parse-crate-line-step1
  ([data] (parse-crate-line-step1 data nil))
  ([data acc-data]
   (println data)
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

(parse-crate-line "    [D]    ")
(parse-crate-line "[Z] [M] [P]")
(parse-crate-line "move 2 from 2 to 1")
;; Split up the data in different categories
(defn parse-data
  [file]
  (let [all-lines (utils/get-lines file)
        crate-lines (take-while #(not (string/includes? % "1")) all-lines)
        move-lines (drop (+ 2 (count crate-lines)) all-lines)]
    {:crates (map parse-crate-line crate-lines)
     :moves (map parse-move-line move-lines)}))

(->> (parse-data "resources/5_input.txt")
     (:crates)
     (map #(nth % 0)))
                                        ; for.
(map #(list (nth % 0)))
;; (map #(conj [] (nth % 0)))




