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
      monkeys (map parse-raw-monkey
                   (filter #(not (= % '("")))
                           (partition-by #(= % "")
                                         (utils/get-lines "resources/input_11.txt")))) 

      monkeys (zipmap (map :id monkeys) monkeys)



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
                           tmp (utils/call op item arg)
                           div (:div monkey)
                           wp (int (/ tmp 3))
                           divisable (int? (/ wp div))
                           target (if divisable
                                    (:true-target monkey)
                                    (:false-target monkey))]

                       (println "  Monkey inspects an item with a worry level of" item) 
                       (println "    Worry level is multiplied by" arg "to" tmp ".")
                       (println "    Monkey gets bored with item. Worry level is divided by 3 to" wp)
                       (if divisable
                         (println "    Current worry level is divisible by" div)
                         (println "    Current worry level is not divisible by" div))
                       (println "    Item with worry level" wp "is thrown to monkey" target ".") 



                       {:monkey (-> monkey
                                    (update :items #(vec (rest %)))
                                    (update :inspect-count inc))
                        :item wp
                        :divisible divisable
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
                ;; "On a single monkey's turn, it inspects and throws all of the items it is holding one at a time and in the order listed."
                (let [turn (:turn state)]
                  (println
                   (apply str "Monkey " turn ":"))
                  (loop [state state]
                    (let [monkeys (:monkeys state)
                          monkey (get monkeys turn)]
                      (if (empty? (:items monkey))
                        (update state :turn inc) ; Next monkey's turn...
                        ;; Otherwise, throw next item.
                        (recur (update state :monkeys 
                                       #(throw-next % monkey))))))))

      print-monkeys (fn [monkeys]
                      (loop [monkeys (vals monkeys)]
                        (let [monkey (first monkeys)]
                          (if (empty? monkeys)
                            nil 
                            (do
                              (println
                               (apply str "Monkey " (:id monkey) ": " (interpose "," (:items monkey))))
                              (recur (rest monkeys))))))
                      monkeys)

      ;; make lazy, inorder to be able to 'take x'
      round (fn round
              ;;"The process of each monkey taking a single turn is called a round."
              [state]
              (let [monkey-count (count (:monkeys state))
                    do-round (fn [state2]
                               (update 
                                (loop [state state2]
                                  (if (>= (:turn state) monkey-count)
                                    (update state :turn #(mod % monkey-count))
                                    (recur (do-turn state))))
                                :round inc))
                    ]
                (lazy-seq (cons state
                                (round (do-round state))))))


      start-state {:monkeys monkeys
                   :turn 0 ; active monkey
                   :round 0 }


      ]

  (vals
   (:monkeys
    (last
     (take 1 (round start-state)))))
  ;;(mod (inc %) (count monkeys))

  (comment 
    (-> start-state
        do-turn
        do-turn
        do-turn
        do-turn
        :monkeys
        print-monkeys))


  (map :inspect-count
       (vals
        (print-monkeys
         (:monkeys
          (first
           (drop 10
                 (round start-state)))))))
  )


