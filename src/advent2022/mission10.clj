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
                                    :else {:noop true })))]
           (map parse-line
                (utils/get-lines "resources/input_10.txt")))

      ops [nil
           nil
           {:addx 23 :op-cycles 4 }]

      initial-cpu {:x 1
                   :cycle 0
                   :ops ops }

      addx-fn (fn[cpu] (let [op (:op cpu)
                             op-cycle-count (:op-cycles op)]
                         (loop [op op]
                           (if (= (:op-cycles op) 0)
                             (-> cpu
                                 (update :x #(+ % (:addx op))) ; effect of op 
                                 (update :cycle #(+ % op-cycle-count)) ; Increase clock cycles of cpu
                                 (dissoc :op)) ; Remove op (it's done)
                             (recur (update op :op-cycles dec))))))

      addx-fn (fn[] (update :ops
                            #(let [[op & other-ops] %
                                   cycles (:op-cycles op)]
                               (println "addx" (cons (update op :op-cycles dec) other-ops))
                               (if (> cycles 1)
                                 (cons
                                  (update op :op-cycles dec)
                                  other-ops)
                                 other-ops))))

      do-cycle (fn do-cycle
                 [cpu]
                 (let [cpu-fn (fn[cpu]
                                (let [ops (:ops cpu)
                                      op (first ops)]

                                  (update 
                                   (cond
                                     (nil? op) (do
                                                 (println "noop" op "CPU:" cpu)
                                                 (-> cpu
                                                     (update :ops rest)))
                                     
                                     (:addx op) (addx-fn cpu)
                                     

                                     :else (-> cpu ; noop
                                               (update :ops rest))

                                     ) :cycle inc)))
                       ] 
                   (if (empty? (:ops cpu)) 
                     []
                     (lazy-seq
                      (cons cpu
                            (do-cycle (cpu-fn cpu)))))))

      ]
  
                                        ;(take 4) 
  (map #(dissoc % :ops) (do-cycle initial-cpu)))


(update op :op-cycles dec)

(let [cpu {:x 1, :cycle 1, :ops [{:addx 2, :op-cycles 2}]}
      op (first (:ops cpu))]

  (if (= (dec (:op-cycles op)) 0)
    ;; operation takes effect
    (-> cpu
        (update :ops rest)
        (update :x #(+ (:addx op) %)))
    ;;
    (-> cpu
        (update-in [:ops 0 :op-cycles] dec))))

;; assoc and update works in vectors too!
(assoc [1 2 3 4] 0 :a)
(update [1 2 3 4] 0 inc)

(update-in 
 {:x 1, :cycle 1, :ops [{:addx 2, :op-cycles 2}]}
 [:ops 0 :op-cycles] inc)


