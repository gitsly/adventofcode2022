(ns advent2022.mission6
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class))

;; jpqm

(let [line (vec (first (utils/get-lines "resources/6_input.txt")))
      size 4]
  (loop [coll line
         taken []
         index 0]
    (let [marker (take size coll)]
      (println marker)
      (if (= (count marker)
             (count(distinct marker)))
        {:taken taken
         :remaining coll
         :index (+ index size)}
        (recur (rest coll) marker (inc index))))))

(def answers [7 5 6 10])
