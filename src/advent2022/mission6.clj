(ns advent2022.mission6
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class))

;; jpqm

(defn find-marker
  [size
   line]
  (println size)
  (loop [coll line
         taken []
         index 0]
    (let [marker (take size coll)]
      (println marker)
      (if (= (count marker)
             (count(distinct marker)))
        {:marker taken
         :remaining coll
         :index (+ index size)}
        (recur (rest coll) marker (inc index))))))

(find-marker 4
             (vec
              (first (utils/get-lines "resources/6_input.txt"))))

(->> (utils/get-lines "resources/6_input_full.txt")
     (map vec)
     (map #(find-marker 14 %))
     (map :index))

(map find-marker 4
     (first ))
