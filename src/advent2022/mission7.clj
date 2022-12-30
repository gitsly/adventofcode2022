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

(defn cd 
  [state
   dir]
  (if (= ".." dir)
    (update state :cwd #(pop %))
    (update state :cwd #(conj % dir))))


(defn ls
  [state
   content]  ; [sample-dir sample-file] list of data (files / dir)
  (let [cwd (:cwd state)
        dirs (:dirs state)]
    (if (empty? cwd)
      state
      (update state :dirs #(merge % { cwd content })))))

;; Test some seqential state shifting
(-> sample-state1
    (cd "somedir")
    (ls [sample-dir sample-file])
    (cd "another")
    (ls [{:apa 1}]))

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
   count)
  
  ;;  {:commands (take-while #(:command %) data)
  ;;   :data (take-while #(:data %) data) }

  )


  (cmd-fn data))

;; Part 1
(let [all-input (->> (utils/get-lines "resources/7_input.txt")
                     (map #(parse-line input-data %)))
build-dir (fn [state
               data]


            state)]
;; Build state from input
(loop [input all-input 
       state {:note "initial state"}]
(if(empty? input)
  state
  (recur (rest input) (build-dir state (first input))))))


