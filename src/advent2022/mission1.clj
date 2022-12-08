(ns advent2022.mission1
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
       reverse))

(take 3 [1 2 3 4])

(reduce + 
        (take 3
              (get-elf-max-calories 
               (map utils/as-integer (utils/get-lines  "resources/1_input.txt")))))
