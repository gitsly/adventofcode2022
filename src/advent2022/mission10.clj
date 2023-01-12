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

      ops(let [parse-line (fn[line]
                            (let [[_ op v] (re-matches #"(.*) (\d*)" line)]
                              (cond v { :addx (utils/as-integer v) :op-cycles 4 }
                                    :else nil)))]
           (vec (map parse-line
                     (utils/get-lines "resources/input_10.txt"))))

      ;;testing
      ;;      ops [nil
      ;;           nil
      ;;           {:addx 23 :op-cycles 4 }]

      initial-cpu {:x 1
                   :cycle 0
                   :ops ops }

      noop-fn (fn [cpu]
                (println "noop")
                (-> cpu
                    (update :ops #(vec (rest %)))))

      addx-fn (fn
                [cpu]
                (let [op (first (:ops cpu))
                      v (:addx op)]
                  (println "addx")
                  (if (= (dec (:op-cycles op)) 0)
                    ;; operation takes effect
                    (do
                      (-> cpu
                          (update :ops #(vec (rest %)))
                          (update :x #(+ (:addx op) %))))
                    ;;
                    (-> cpu
                        (update-in [:ops 0 :op-cycles] dec)))))

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

                   (println "do-cycle" cpu)
                   (lazy-seq (cons cpu (do-cycle (cpu-fn cpu))))))

      signal-strength (fn[cpu] (* (:x cpu) (:cycle cpu)))

      ]
  
  ;; (map signal-strength) 
  ;; (take-while #(not (empty? (:ops %))) (do-cycle initial-cpu))

  ;;  (map #(dissoc % :ops) (take 8 (do-cycle initial-cpu)))

  (map
   ;; #({:signal-strength (signal-strength %) })
   #(:cycle %)

   (take-nth 40
             (drop 20 (take (+ 40 220) (do-cycle initial-cpu)))))
  ;;  (:ops initial-cpu)
  )
