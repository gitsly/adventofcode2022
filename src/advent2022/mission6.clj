(ns advent2022.mission6
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class))

(defn find-marker
  [size
   line]
  (loop [coll line
         taken []
         index 0]
    (let [marker (take size coll)]
      (if (= (count marker)
             (count(distinct marker)))
        {:marker taken :remaining coll :index (+ index size)}
        (recur (rest coll) marker (inc index))))))

;; Part 1 -> 1582
(:index (find-marker 4 (vec (first (utils/get-lines "resources/6_input_full.txt")))))

;; Part 2 -> 3588
(:index (find-marker 14 (vec (first (utils/get-lines "resources/6_input_full.txt")))))

