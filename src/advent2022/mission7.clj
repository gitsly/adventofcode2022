(ns advent2022.mission7
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
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

(def initial-state {:cwd [] :note "initial state" :counter 0 } )
(def sample-state1 {:cwd [] :note "sample state 1"} )
(def sample-dir (parse-line input-data "dir a"))
(def sample-file (parse-line input-data "62596 h.lst"))
(def sample-file2 (parse-line input-data "213 j.lst"))

(println sample-dir)
(println sample-file)

(defn cd 
  [state
   dir]
  (if (= ".." dir)
    (update state :cwd #(pop %))
    (-> state
        (update :cwd #(conj % (keyword dir)))
        (assoc :content []))))


(defn ls
  "null-op: add as encountered after ls instead" 
  [state]
  state)

(defn file
  [state
   f]
  (let [a (:cwd state)])
  (update-in state [:content ]  #(conj % f)))


(defn dir
  [state
   d]
  state)

;; Test some seqential state shifting
(-> initial-state
    (cd "/")
    ls
    (dir sample-dir)
    (file sample-file)
    (file sample-file2)
    (cd "another")
    ls)




(let [state sample-state1
      content []

      cwd (:cwd state)
      dirs (:dirs state)]
(if (empty? cwd)
  state
  (update state :dirs #(merge % { cwd content }))))

(def sample-data1
  {:dir "/"
   :content [{:dir "a"
              :content [{:dir "e" }
                        {:file "b.txt" :size 14848514 }
                        {:file "c.dat" :size 8504156 }
                        ]}]})

(def sample-data2
  {:/
   [{:a
     [{:e []}
      "f" "g" "h.lst"]}
    "b.txt", "c.dat"
    {:d
     []}]})

(def sample-data3a
  {:/ [{:a [{:e [{:file "i" :size 584}]}
            {:file "f" :size 29116}
            {:file "g" :size 29116}
            {:file "h.lst" :size 29116}]}
       {:file "b.txt" :size 14848514}
       {:file "c.dat" :size 8504156}
       {:d [{:file "j" :size 4060174}
            {:file "d.log" :size 8033020}
            {:file "d.ext" :size 5626152}
            {:file "k" :size 7214296}]}]})

;; This structure should work nice with built in 
(def sample-data3
  {:/ 
   {:a
    {"d.log" {:size 8033020}}
    "d.ext" {:size 5626152}
    "k" {:size 7214296}}})

(keys (:/ sample-data3))

(get-in sample-data3 [:/ :a]); working

(walk/prewalk-demo sample-data3)

(+ 14848514
8504156)

(defn size-of-files-in-dir
[dir]
(reduce + (map :size
               (filter :file (:content dir)))))


(size-of-files-in-dir (first (:content sample-data1)))




                                        ; Total size of Dir 'a
(+ 584 29116 2557 62596)

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
                     (map #(parse-line input-data %))
                     (take 10))

      init-state initial-state
      build-dir (fn [state
                     data]
                  (-> state
                      (update :counter inc)
                      (update :data #(conj data %))))]
;; Build state from input
(loop [input all-input 
       state init-state]
  (cond
    (empty? input) state ; done
    :else (recur
           (rest input)
           (build-dir state (first input))))))


