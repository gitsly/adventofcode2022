(ns utils.utils
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]))

(defn as-integer
  "Tries to convert string value to integer, if not possible, returns nil"
  [d]
  (try (Integer/parseInt d)
       (catch Exception e nil)))

(defn get-lines
  [file-path]
  (line-seq
   (clojure.java.io/reader file-path)))

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
