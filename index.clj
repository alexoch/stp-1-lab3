(require
 '[clojure.java.io :as io]
 '[clojure.string :as string])

(defn subs-end [line start len]
  (string/reverse (subs (string/reverse line) start len)))
(defn get-type [fname] (subs-end fname 0 3))
(defn get-delim [type] (if (= type "csv") #"," #"\t"))
(defn split-row [str type] (string/split str (get-delim type)))

(defn get-rows [fname] (string/split-lines (slurp fname)))

(defn get-lists [fname]
  (map #(split-row % (get-type fname))
       (string/split-lines (slurp (str "files/" fname)))))

(def files ["skl1.tsv", "skl2.csv", "posts.csv", "assistants.csv"])
(def lengths  ['(11 11 8 10 10 10),
               '(4 4 6 6 35 5 5),
               '(5 35 50 10 35 45 40 30 20 10 5 6 60 60 60 60),
               '(5 35 6)])

(defn get-len [fname i] (nth (nth lengths (.indexOf files fname)) i))

(defn col-to-str [col len]
  (str "| " col (apply str (repeat (- len (count col)) " ")) " "))

(defn output-row [row fname]
  (println
   (apply str
          (map-indexed
            #(col-to-str (if (< (count %2) 55) %2 "long value") (get-len fname %)) row))))

(defn output-by-line [fname]
  (let [type (get-type fname)]
    (with-open [r    (io/reader (io/file (str "files/" fname)))]
      (doseq [line (line-seq r)] (output-row (split-row line type) fname)))))

(defn get-filename [line] (subs-end (subs line 6) 2 (- (count line) 6)))

(println "Files available: " (string/join ", " files))
(println "Enter command:")

(let [cli-input (string/trim (read-line))]
  (if (and (< 6 (count cli-input)) (= (subs cli-input 0 5) "load("))
    (let [fname (get-filename cli-input)]
      (if (< (.indexOf files fname) 0)
        (println "File doesn't exist")
        (output-by-line fname)))
    (println "Wrong command")))
