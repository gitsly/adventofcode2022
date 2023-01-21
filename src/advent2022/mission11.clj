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
                            :op [(keyword op) (utils/as-integer arg)]})
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
                       {:monkey (update monkey :items #(vec (rest %)))
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

      turn (fn turn [state]
             ;; "On a single monkey's turn, it inspects and throws all of the items it is holding one at a time and in the order listed."
             (let [monkey (:turn state)]
               (update state :monkeys inc)))

      ;; make lazy, inorder to be able to 'take x'
      round (fn round
              ;;"The process of each monkey taking a single turn is called a round."
              [state]
              (let [do-round (fn
                               ;; Actual update of state
                               [state]
                               (turn state))]
                (lazy-seq (cons state (round (do-round state))))))

      start-state {:monkeys monkeys
                   :turn 0 ; active monkey
                   }]

  

  ;; (last 
  ;;  (take 3 (round start-state)))

  (let [state start-state
        monkeys (:monkeys state)
        turn (:turn state)]
    (println
     (apply str "Monkey " turn ":"))
    (update state :monkeys 
            #(throw-next % (get monkeys turn))))


  ;;  
  
  )



(comment "
Monkey 0:
  Monkey inspects an item with a worry level of 79.
    Worry level is multiplied by 19 to 1501.
    Monkey gets bored with item. Worry level is divided by 3 to 500.
    Current worry level is not divisible by 23.
    Item with worry level 500 is thrown to monkey 3.
  Monkey inspects an item with a worry level of 98.
    Worry level is multiplied by 19 to 1862.
    Monkey gets bored with item. Worry level is divided by 3 to 620.
    Current worry level is not divisible by 23.
    Item with worry level 620 is thrown to monkey 3.
")

