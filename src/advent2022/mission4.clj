(ns advent2022.mission4
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.set :as set])
  (:gen-class))

                                        ; In how many assignment pairs does one range fully contain the other?

(re-matches #"(hello)" "hello")

(defn section-range
  [from to]
  (range (int from) (int (inc to))))


(defn parse-line 
  [input]
  (let [matches (re-matches #"(\d+)-(\d+),(\d+)-(\d+)", input)
        matches-int (map utils/as-integer (drop 1 matches)) 
        [a1 a2 b1 b2] matches-int]
    {:a (section-range a1 a2)
     :b (section-range b1 b2) }))


(map parse-line (utils/get-lines "resources/4_input.txt"))

;; full-overlap
()
