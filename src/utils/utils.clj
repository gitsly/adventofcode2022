(ns utils.utils
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]))

(defn as-integer
  "Tries to convert string value to integer, if not possible, returns nil"
  [d]
  (try (Integer/parseInt d)
       (catch Exception e nil)))

(defn get-lines
  [file-path]
  (line-seq
   (clojure.java.io/reader file-path)))

(defn parse-int-list
  "parse string list and returns list of integers 1,  2,  3 78 90 -12"
  [d]
  (map utils/as-integer (string/split d #",?\s+")))

(defn dups
  "Return duplicates in sequence"
  [seq]
  (for [[id freq] (frequencies seq)  ;; get the frequencies, destructure
        :when (> freq 1)]            ;; this is the filter condition
    id))                              ;; just need the id, not the frequency

;;(let [data-structure {:test 'something}]
;;  (binding [*print-dup* true] (prn data-structure)))


;;(defn serialize
;;  "Print a data structure to a file so that we may read it in later."
;;  [data-structure #^String filename]
;;  (with-out-writer
;;    (java.io.File. filename)
;;    (binding [*print-dup* true] (prn data-structure))))

;;(with-open [w (clojure.java.io/writer  "f:/w.txt" :append true)]
;;  (.write w (str "hello" "world"))))

;; This allows us to then read in the structure at a later time, like so:
;;(defn deserialize [filename]
;;(with-open [r (PushbackReader. (FileReader. filename))]
;;  (read r)))
