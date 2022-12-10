(ns advent2022.mission5
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.set :as set])
  (:gen-class))

(defn parse-line 
  [input]
  (let [matches (re-matches #"(\d+)-(\d+),(\d+)-(\d+)", input)
        matches-int (map utils/as-integer (drop 1 matches)) 
        [a1 a2 b1 b2] matches-int]
    nil))
