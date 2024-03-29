(ns advent2022.mission11
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))



(let [parse-raw-monkey (fn[raw-monkey]
                         (let [[id-line
                                items-line
                                operation-line
                                test-line
                                test-line-true
                                test-line-false] raw-monkey
                               [_ id] (re-matches           #"Monkey (.*):" id-line)
                               [_ items] (re-matches        #"\s*Starting items: (.*)" items-line)
                               [_ op arg] (re-matches       #"\s*Operation: new = old (.*) (.*)" operation-line)
                               [_ div] (re-matches          #"\s*Test: divisible by (.*)" test-line)
                               [_ true-target] (re-matches  #"\s*If .*: throw to monkey (.*)" test-line-true)
                               [_ false-target] (re-matches #"\s*If .*: throw to monkey (.*)" test-line-false)]

                           {:id (utils/as-integer id)
                            :items (vec (utils/parse-int-list items))
                            :inspect-count 0 ; How many times a monkey has inspected and thrown items
                            :div (utils/as-integer div)
                            :true-target (utils/as-integer true-target)
                            :false-target (utils/as-integer false-target)
                            :op [(keyword op)
                                 (utils/as-integer arg) ; Note. if 'old' as string then this will be nil, checked in calc func later
                                 ]})
                         )


      file "resources/input_11.txt"  ; 10605 (Part I)
      file "resources/input11_full.txt" ;Your puzzle answer was 100345 (Part I).

      monkeys (map parse-raw-monkey
                   (filter #(not (= % '("")))
                           (partition-by #(= % "")
                                         (utils/get-lines file)))) 

      monkeys (zipmap (map :id monkeys) monkeys)

      least-common-multiplier (reduce utils/lcm 
                                      (map :div (-> monkeys vals)))

      receive-item (fn [monkey
                        item]
                     ;; When a monkey throws an item to another monkey, the item goes on the end of the recipient monkey's list.
                     ;; item is worry-points (integer)
                     (update monkey :items #(conj % item)))

      inspect-item (fn
                     ;; Returns new :monkey and updated :item
                     [monkey]
                     (let [items (:items monkey)
                           item (first items)
                           [op arg] (:op monkey)
                           arg (if arg
                                 arg
                                 item)
                           tmp (utils/call op (bigint item) (bigint arg))
                           div (:div monkey)

                           ;;wp  (mod (bigint (/ tmp 3)) least-common-multiplier) ; Part I

                           wp  (mod tmp least-common-multiplier) ; Part I

                           divisible (utils/divisible? wp div)
                           target (if divisible
                                    (:true-target monkey)
                                    (:false-target monkey))]

                       (comment
                         (println "  Monkey inspects an item with a worry level of" item) 
                         (println "    Worry level is multiplied by" arg "to" tmp ".")
                         (println "    Monkey gets bored with item. Worry level is divided by 3 to" wp)
                         (if divisible
                           (println "    Current worry level is divisible by" div)
                           (println "    Current worry level is not divisible by" div))
                         (println "    Item with worry level" wp "is thrown to monkey" target "."))
                       



                       {:monkey (-> monkey
                                    (update :items #(vec (rest %)))
                                    (update :inspect-count inc))
                        :item wp
                        :divisible divisible
                        :target target}))

      throw-next (fn [monkeys
                      thrower]
                   (let [throw    (inspect-item thrower)
                         thrower  (:monkey throw)
                         target   (:target throw)
                         receiver (get monkeys target)]
                     (-> monkeys
                         (assoc-in [(:id thrower)] thrower)
                         (assoc-in [target] 
                                   (receive-item receiver (:item throw))))))

      do-turn (fn do-turn
                [state]
                ;; "On a single monkey's turn, it inspects and throws
                ;; all of the items it is holding one at a time and in
                ;; the order listed."
                (let [turn (:turn state)]

                  (comment
                    (println
                     (apply str "Monkey " turn ":")))
                  (loop [state state]
                    (let [monkeys (:monkeys state)
                          monkey (get monkeys turn)]
                      (if (empty? (:items monkey))
                        (update state :turn inc) ; Next monkey's turn...
                        ;; Otherwise, throw next item.
                        (recur (update state :monkeys 
                                       #(throw-next % monkey))))))))


      seq-monkeys (fn [monkeys
                       seq-fn]
                    (loop [monkeys (vals monkeys)]
                      (let [monkey (first monkeys)]
                        (if (empty? monkeys)
                          nil 
                          (do
                            (seq-fn monkey)
                            (recur (rest monkeys))))))
                    monkeys)

      print-monkeys (fn [monkeys]
                      (seq-monkeys monkeys
                                   (fn [monkey]
                                     (println
                                      (apply str "Monkey " (:id monkey) ": " (interpose "," (:items monkey)))))))

      print-monkeys-inspect (fn [monkeys]
                              (seq-monkeys monkeys
                                           (fn [monkey]
                                             (println "Monkey" (:id monkey)
                                                      "inspected items"(:inspect-count monkey)
                                                      "times."))))

      ;; make lazy, inorder to be able to 'take x'
      round (fn round
              ;;"The process of each monkey taking a single turn is called a round."
              [state
               round-count]
              (let [monkey-count (count (:monkeys state))
                    do-round (fn [state]
                               (update 
                                (loop [state state] ; Loop and perform each monkey's business (inspect all items)
                                  (if (>= (:turn state) monkey-count)
                                    (update state :turn #(mod % monkey-count))
                                    (recur (do-turn state))))
                                :round inc))]
                (loop [round 0
                       state state]
                  (if (>= round round-count)
                    state
                    (recur (inc round)
                           (do-round state))))))


      start-state {:monkeys monkeys
                   :turn 0 ; active monkey
                   :round 0 }]

  (let [round-count 20

        round-count 10000

        end-state (round start-state round-count)


        end-monkeys (map #(select-keys % [:id
                                          :inspect-count
                                          :items])
                         (vals (:monkeys end-state)))

        top-two-monkeys (take 2 (reverse (sort-by #(:inspect-count %) end-monkeys)))

        monkey-business (apply * (map :inspect-count top-two-monkeys)) 
        ] 
    (time
     (print-monkeys-inspect
      (:monkeys end-state)))

    ;; Part II: 28537348205
    (println monkey-business)))
  

