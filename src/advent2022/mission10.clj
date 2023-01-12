(ns advent2022.mission10
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))


(let [fn1 (fn fn1
            ([a] a)
            ([] nil))]
  [(fn1 2)
   (fn1)])


(let [

      ops (let [parse-line (fn[line]
                             (let [[_ op v] (re-matches #"(.*) (-?\d*)" line)]
                               (cond v { :addx (utils/as-integer v) :op-cycles 2 }
                                     :else nil)))]
            (vec (map parse-line
                      (utils/get-lines "resources/input_10_full.txt"))))

      ;;testing
      ;;      ops [nil
      ;;           nil
      ;;           {:addx 23 :op-cycles 2 }]

      initial-cpu {:x 1
                   :cycle 1
                   :ops ops }

      noop-fn (fn [cpu]
                ;;(println (:cycle cpu) "noop")
                (-> cpu
                    (update :ops #(vec (rest %)))))

      addx-fn (fn
                [cpu]
                (let [op (first (:ops cpu))
                      v (:addx op)]
                  (if (= (dec (:op-cycles op)) 0)
                    ;; operation takes effect
                    (do
                      (-> cpu
                          (update :ops #(vec (rest %)))
                          (update :x
                                  (fn[x]
                                    ;;(println (:cycle cpu)"add"(:addx op) "to" x)
                                    (+ (:addx op) x))

                                  )))
                    ;;
                    (do
                      ;;(println (:cycle cpu)"add...")
                      (-> cpu
                          (update-in [:ops 0 :op-cycles] dec))))))

      do-cycle (fn do-cycle
                 [cpu]
                 (let [cpu-fn (fn[cpu]
                                (let [ops (:ops cpu)
                                      op (first ops)]
                                  (-> (cond
                                        (nil? op) (noop-fn cpu)
                                        (:addx op) (addx-fn cpu)
                                        :else (-> cpu ; noop (or no more instructions)
                                                  (update :ops #(vec (rest %)))))
                                      (update :cycle inc))))] 

                   ;;(println "do-cycle" cpu)
                   (lazy-seq (cons cpu (do-cycle (cpu-fn cpu))))))

      signal-strength (fn[cpu] (* (:x cpu)
                                  (:cycle cpu)))

      prep-data (fn[cpu]
                  (-> cpu
                      (select-keys [:cycle :x])
                      (assoc :ss (signal-strength cpu))))

      ;; 12470 (too low)
      part1-solution (apply + 
                            (map :ss
                                 (map prep-data
                                      (take 6
                                            (take-nth 40
                                                      (drop (dec 20) (do-cycle initial-cpu)))))))
      ]

  (map :ss
       (map prep-data
            (take 6
                  (take-nth 40
                            (drop (dec 20) (do-cycle initial-cpu)))))))
