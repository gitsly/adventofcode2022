(ns utils.utils)

(defn as-integer
  "Tries to convert string value to integer, if not possible, returns nil"
  [d]
  (try (Integer/parseInt d)
       (catch Exception e nil)))

(defn get-lines
  [file-path]
  (line-seq
   (clojure.java.io/reader file-path)))
