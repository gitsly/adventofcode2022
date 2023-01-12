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
                                        :else (-> cpu ; noop
                                                  (update :ops #(vec (rest %)))))
                                      (update :cycle inc))))] 

                   (println "do-cycle" cpu)
                   (lazy-seq
                    (if (empty? (:ops cpu)) 
                      cpu
                      (cons cpu
                            (do-cycle (cpu-fn cpu)))))))

      ]
  
  (map #(dissoc % :ops) (do-cycle initial-cpu))

  ;;(addx-fn {:x 1, :cycle 1, :ops [{:addx 23, :op-cycles 1}]})

  )

(defn positive-numbers 
  ([] (positive-numbers 1))
  ([n] (lazy-seq (cons n (positive-numbers (inc n))))))

(take 3 (positive-numbers))

(defn fib
  ([] (fib 1 1))
  ([a b] (lazy-seq
          (cons a (fib (+ a b) a)))))

(take 10 (fib))

(defn do-cycle
  [cpu]
  (println "loop" cpu)
  (let [cycle-fn (fn[cpu]
                   (-> cpu
                       (update :ops #(vec (rest %)))
                       (update  :cycle inc)))]
    (lazy-seq (cons cpu (do-cycle (cycle-fn cpu))))))


(take-while #(not (empty? (:ops %)))
            (do-cycle {:note "some state" :cycle 0 :ops [1 2 3]}))


(distinct
 (map :done
      (take 130 (do-cycle {:note "state1"}))))

