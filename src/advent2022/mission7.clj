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

(def command-list [{:regex #"\$ cd (.*)"
                    :transform (fn [input]
                                 (when-let [[_ arg] input]
                                   { :command 'cd
                                    :arg arg}) )}
                   {:regex #"\$ ls"
                    :transform (fn [input] (when-let [_ input] {:command 'ls }))}])

(when-let [apa 2] "a")


(def sample-dir {:total-size 12
                 :wd "/"})

(defn build-dir
  [state]
  state)

(defn parse-line
  [line
   command-list])

(defn first-not-nil
  [coll]
  (first (filter #(not (nil? %)) coll)))


(let [commands command-list
      line "$ cd /"
      ;;line "$ ls"
      ]
  (first-not-nil
   (map #(let [cmd %
               pattern (:regex cmd)
               transform (:transform cmd)]
           (transform (re-matches pattern line))) commands)))



(parse-line "$ cd /" command-list))) 

;; Part 1 -> 1582
(let [all-lines (utils/get-lines "resources/7_input.txt")]
;; Build state from input
(loop [lines all-lines
state {:note "initial state"}]

(if(empty? lines)
state
(recur (rest lines) (build-dir state)))))


(utils/get-lines "resources/7_input.txt")
