(ns advent2022.mission4
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.set :as set])
  (:gen-class))


(defn section-range
  [from to]
  (range (int from) (int (inc to))))

(defn parse-line 
  [input]
  (let [matches (re-matches #"(\d+)-(\d+),(\d+)-(\d+)", input)
        matches-int (map utils/as-integer (drop 1 matches)) 
        [a1 a2 b1 b2] matches-int]
    {:a (section-range a1 a2)
     :b (section-range b1 b2) }))

(defn max-count
  [a b]
  (if (> (count a) (count b))
    a
    b))

(defn into-sets
  [data]
  {:a (set (:a 
            (parse-line data)))
   :b (set (:b (parse-line data)))})

(defn fully-contains
  [a b]
  (=
   (into (hash-set) (set/union a b))
   (into (hash-set) (max-count a b))))

(defn overlap-at-all
  [a b]
  (empty? (set/intersection (into (hash-set) a)
                            (into (hash-set) b))))

;; In how many assignment pairs does one range fully contain the other?

;; (let [data (parse-line "5-7,7-9")
;;       a (:a data)
;;       b (:b data)]
;;   (empty? (set/intersection (into (hash-set) a)
;;                             (into (hash-set) b))))


(->> (map #(fully-contains (:a %) (:b %))
          (->>
           (utils/get-lines "resources/4_input_full.txt")
           (map parse-line)))
     (filter #(= % true))
     count)

;; Part 2
;; In how many assignment pairs does one range fully overlap-at-all?
(->> (map #(overlap-at-all (:a %) (:b %))
          (->>
           (utils/get-lines "resources/4_input.txt")
           (map parse-line)))
     (filter #(= % true))
     count)
