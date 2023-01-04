(ns advent2022.trees
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))


(defn branch?
  "Return true if node 'can' contain children"
  [node]
  (not (nil? (:children node))))

(defn children
  "Returns vector of children for passed 'node'"
  [node]
  (:children node))

(def root {:children [{:children []
                       :name "child1"}
                      {:children []
                       :name "child2"}]
           :name "testroot"})

(tree-seq branch? children root)

;; Simple test
(def mymap {:hello 5 :test {:hello 2}})
;;You can get the lazy sequence of its nodes like so:
(def tree-lazy-seq
  (tree-seq map? vals mymap))
;;=> ({:hello 5, :test {:hello 2}} ;root node
;;    5                            ;first leaf
;;    {:hello 2}                   ;second child
;;    2)                           ;child's leaf


(walk/prewalk-demo tree-lazy-seq)

;; Below is same as above
(walk/prewalk
 (fn [node]
   (println node)
   node) tree-lazy-seq)
