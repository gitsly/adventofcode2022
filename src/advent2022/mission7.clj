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
                                 {:cd arg}) )}
                 {:regex #"\$ ls"
                  :transform (fn [input]
                               (when-let [_ input] {:ls true}))}

                 {:regex #"dir (.*)"
                  :transform (fn [input]
                               (when-let [[_ arg] input] {:dir arg
                                                          :content []}))}

                 {:regex #"(\d*) (.*)"
                  :transform (fn [input]
                               (when-let [[_ size file] input] {:file file
                                                                :size (utils/as-integer size)}))}

                 ])

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


(when-let [test (:command (parse-line input-data "cd /"))]
  test) 

(def initial-state {:cwd [] :note "initial state"} )
(def sample-state1 {:cwd [] :note "sample state 1"} )
(def sample-dir (parse-line input-data "dir a"))
(def sample-file (parse-line input-data "62596 h.lst"))

(println sample-dir)
(println sample-file)

(defn cd 
  [state
   dir]
  (if (= ".." dir)
    (update state :cwd #(pop %))
    (update state :cwd #(conj % dir))))


(defn ls
  "null-op: add as encountered after ls instead" 
  [state]
  state)

(defn file
  [state
   f]
  state)

(defn dir
  [state
   d]
  state)

(let [state sample-state1
      content []

      cwd (:cwd state)
      dirs (:dirs state)]
  (if (empty? cwd)
    state
    (update state :dirs #(merge % { cwd content }))))

{:dir "/"
 :content [{:file "b.txt" :size 14848514 }
           {:dir "a"
            :content [{:dir "e" }
                      {:file "b.txt" :size 14848514 }]}]}


;; Test some seqential state shifting
(-> initial-state
    (cd "/")
    ls
    (dir sample-dir)
    (file sample-file)
    (cd "another")
    ls)


;;(= sample-state1 (cd (cd sample-state1 "heppas") "..")) ;-> true


;; possible to use list as a key into map
;;(get { ["a" "b"] 1 } ["a" "b"] )

(let [data (->> (utils/get-lines "resources/7_input.txt")
                (map #(parse-line input-data %)))
      spl (split-with #(:command %) data)
      commands (map :command spl)

      [commands dtr] spl]

(->>
 {:commands commands
  :dtr (take-while #(:data %) dtr) }
 :dtr
 count))



;; Part 1
(let [all-input (->> (utils/get-lines "resources/7_input.txt")
                     (map #(parse-line input-data %)))
      init-state initial-state
      build-dir (fn [state
                     data]


                  state)]
;; Build state from input
(loop [input all-input 
       state init-state]
  (if(empty? input)
    state
    (recur (rest input) (build-dir state (first input))))))


