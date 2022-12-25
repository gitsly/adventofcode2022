(ns advent2022.mission7
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class))

(comment "
- / (dir)
  - a (dir)
    - e (dir)
      - i (file, size=584)
    - f (file, size=29116)
    - g (file, size=2557)
    - h.lst (file, size=62596)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
  - d (dir)
    - j (file, size=4060174)
    - d.log (file, size=8033020)
    - d.ext (file, size=5626152)
    - k (file, size=7214296)")

(def input-data [{:regex #"\$ cd (.*)"
                  :transform (fn [input]
                               (when-let [[_ arg] input]
                                 { :command :cd
                                  :arg arg}) )}
                 {:regex #"\$ ls"
                  :transform (fn [input]
                               (when-let [_ input] {:command :ls }))}

                 {:regex #"dir (.*)"
                  :transform (fn [input]
                               (when-let [[_ arg] input] {:data :dir :arg arg}))}

                 {:regex #"(\d*) (.*)"
                  :transform (fn [input]
                               (when-let [[_ size file] input] {:data :file
                                                                :size size
                                                                :file file}))}

                 ])

(def sample-dir {:total-size 12
                 :wd "/"})



(defn first-not-nil
  [coll]
  (first (filter #(not (nil? %)) coll)))


(defn parse-line
[commands
 line]
(first-not-nil
 (map #(let [input-data %
             pattern (:regex input-data)
             transform (:transform input-data)]
         (transform (re-matches pattern line))) commands)))


(parse-line input-data "$ cd /")
(parse-line input-data "$ ls")
(parse-line input-data "dir a")
(contains? (parse-line input-data "62596 h.lst") :data)

(when-let [test (:command (parse-line input-data "cd /"))]
  test) 


(let [data (parse-line input-data "$ cd /")
      command-fns {:cd (fn [arg] nil)}
      cmd-fn (:command command-fns)]
  
  (cmd-fn data))

;; Part 1 -> 1582
(let [all-input (->> (utils/get-lines "resources/7_input.txt")
                     (map #(parse-line input-data %)))
      build-dir (fn [state
                     data]

                  (if ()) 

                  state)]
  ;; Build state from input
  (loop [input all-input 
         state {:note "initial state"}]
    (if(empty? input)
      state
      (recur (rest input) (build-dir state (first input))))))
