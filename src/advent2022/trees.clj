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

(walk/prewalk-demo root)
