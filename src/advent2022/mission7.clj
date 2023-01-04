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
                               (when-let [[_ arg] input] {:dir arg }))}

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

(def initial-state {:cwd [] :note "initial state" :counter 0 :filesystem {} :dirs #{} } )
(def sample-state1 {:cwd [] :note "sample state 1"} )
(def sample-dir (parse-line input-data "dir a"))
(def sample-file (parse-line input-data "62596 h.lst"))
(def sample-file2 (parse-line input-data "213 j.lst"))

(println sample-dir)
(println sample-file)

(defn cd 
  [state
   d]
  (let [dir (:cd d)
        cwd (:cwd state)]
    ;;    (println (string/join (map name (:cwd state)))"> cd" dir )
    (update (if (= ".." dir)
              (update state :cwd #(pop %))
              (-> state
                  (update :cwd #(conj % (keyword dir)))))
            :dirs #(if (empty? cwd)
                     %
                     (conj % cwd))))))

(cd initial-state {:cd "/"})


(conj 
(conj #{} [:a])
[:a :b])


(conj #{[:/ :a] [:/]} [:/]) 

(conj #{[:/ :a] [:/]} []) 

(defn ls
"null-op: add as encountered after ls instead" 
[state]
state)

(defn file
[state
 f]
(let [path (concat [:filesystem] (:cwd state))]
  (update-in state path
             #(merge % {(:file f) {:size (:size f)}}))))

(defn dir [state
           dir]
(let [path (concat [:filesystem] (:cwd state))]
  (update-in state path
             #(merge % {(keyword (:dir dir)) {}}))))

;; Test some seqential state shifting

(-> initial-state
(cd {:cd "/"})
ls
(dir sample-dir)
(file sample-file)
(file sample-file2)
(cd {:cd "a"})
(file {:file "testfile.txt" :size 1231})
ls

:filesystem)


;;(update-in {:a {:b "test"}} [:a] #(cons % "a" ))

(let [state sample-state1
      content []

      cwd (:cwd state)
      dirs (:dirs state)]
(if (empty? cwd)
  state
  (update state :dirs #(merge % { cwd content }))))

;; This structure should work nice with built in 
(def sample-data {
                  :/ {
                      :a {
                          :e {
                              "i" {:size 584}
                              }
                          "f" {:size 29116}
                          "g" {:size 2557}
                          "h.lst" {:size 62596}
                          }
                      "b.txt" {:size 14848514}
                      "c.dat" {:size 8504156}

                      :d {"j" {:size 4060174}
                          "d.log" {:size 8033020}
                          "d.ext" {:size 5626152}
                          "k" {:size 7214296}}
                      }
                  })


(-> sample-data
:/
keys)

(get-in sample-data [:/ :a :e]); -> i file

;; Get count of 'files' with :size in directory '/a'
(count
(filter #(:size %)
        (vals (get-in sample-data [:/ :a]))))

(count (get-in sample-data [:/ :d])); -> 4 files

(tree-seq map? vals sample-data)



(+ 14848514
8504156)



(size-of-files-in-dir (first (:content sample-data1)))



;; Part 1
(let [all-input (->> (utils/get-lines "resources/7_input.txt")
                     (map #(parse-line input-data %)))

      init-state initial-state

      build-dir (fn build-dir
                  [state
                   d]
                  (cond
                    (:cd d) (cd state d)
                    (:dir d) (dir state d)
                    (:file d) (file state d)
                    :else state))


      size-of-files-in-dir (fn size-of-files-in-dir
                             [d]
                             (reduce + (map :size (filter :size (vals d)))))

      processed  (loop [input all-input 
                        state init-state]
                   (if (empty? input)
                     state ; done
                     (recur
                      (rest input)
                      (build-dir state (first input)))))

      filesystem (:filesystem processed)

      size-of-dir (fn [d]
                    (reduce + 
                            (->>
                             (filter #(:size %) (tree-seq map? vals d) )
                             (map :size))))

      root (get-in filesystem [:/])

      test-size (size-of-dir (get-in filesystem [:/ :d]))

      dirs (:dirs processed)
      dir-sizes (map (fn[d]
                       {:dir d :size (size-of-dir (get-in filesystem d))})
                     dirs)]
  (reduce + 
          (map :size
               (filter #(<= (:size %) 100000) dir-sizes))))





