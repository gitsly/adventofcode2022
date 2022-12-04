(ns advent2022.core
  (:require [clojure.java.io :as io]
            [utils.utils :as utils])
  (:gen-class))

;; test
(utils/as-integer "1231")
(utils/get-lines "resources/1_input.txt")



(defn get-elf-max-calories[col]
  (->> col
       (partition-by nil?)
       (remove #(= % '(nil)))
       (map #(reduce + %))
       sort
       last))

(get-elf-max-calories 
 (map utils/as-integer (utils/get-lines  "resources/1_input.txt")))


(defn -main
  "Advent Of Code, 2022 mission 1"
  [& args]
  (get-elf-max-calories data) )
