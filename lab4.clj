(require
 '[clojure.java.io :as io]
 '[clojure.string :as string])

(def MAX-VAL-LEN 55)
(def files ["skl1.tsv", "skl2.csv", "posts.csv", "assistants.csv"])
(def lengths  ['(11 11 8 10 10 10),
               '(4 4 6 6 35 5 5),
               '(5 35 50 10 35 45 40 30 20 10 5 6 60 60 60 60),
               '(5 35 6)])

(defn get-type [filename] (nth (string/split filename #"\.") 1))
(defn get-delim [filename] (if (= (get-type filename) "csv") #"," #"\t"))
(defn trim-comma [str]
  (if (.endsWith str ",") (.substring str 0 (dec (count str))) str))

(defn col-to-str [col len]
  (str "| " col (apply str (repeat (- len (count col)) " ")) " "))

(defn get-file-cols [filename]
  (string/split
   (with-open [rdr (io/reader (str "files/" filename))]
     (nth (line-seq rdr) 0))
   (get-delim filename)))

(defn output-list-in-line [list col-lengths]
  (println
   (apply str
          (map-indexed
           #(col-to-str (if (< (count %2) MAX-VAL-LEN) %2 "long value") (nth col-lengths %))
           list))))

(defn output-by-line [fname lists cols]
  (let [delim       (get-delim fname),
        col-lengths (nth lengths (.indexOf files fname))]
    (output-list-in-line cols col-lengths)
    (doseq [list lists]
      (output-list-in-line list col-lengths))))

(defn are-rows-cols-same [short-row long-row] (= (.indexOf (map-indexed #(= (nth long-row %) %2) short-row) false) -1) )

(defn shorter-list [list1 list2] (if (> (count list1) (count list2)) list2 list1))
(defn longer-list [list1 list2] (if (> (count list1) (count list2)) list1 list2))

(defn are-rows-same [row1 row2] (are-rows-cols-same (shorter-list row1 row2) (longer-list row1 row2)))

(defn is-distinct [row row-index lists]
  (let [same-rows (filter #(are-rows-same % row) lists)]
    (or (= (count same-rows) 1) (= (.indexOf lists (nth same-rows 0)) row-index))))

(defn filter-distinct [lists]
  (println "filtering...") (remove nil? (map-indexed #(if (is-distinct %2 % lists) %2 nil) lists)))

(defn command-exists [line]
  (or (= line "exit")
      (and (string/includes? line "select") (string/includes? line "from"))))

(defn get-filename [words]
  (let [from-index (.indexOf words "from")]
    (nth words (inc from-index))))

(defn get-query-cols [words]
  (map trim-comma
       (subvec words (if (= (nth words 1) "distinct") 2 1) (.indexOf words "from"))))

(defn get-row-col [row index]
  (if (< (count row) (inc index)) nil (nth row index)))

(defn get-cols-from-row [row indexes]
  (map #(get-row-col row %) indexes))

(defn get-lists [filename]
  (map #(string/split % (get-delim filename))
       (rest (string/split-lines (slurp (str "files/" filename))))))

(defn is-between [val range]
  (and (> (Integer. val) (nth range 0)) (< (Integer. val) (nth range 1))))

(defn filter-where [filename conditions]
  (let [lists (get-lists filename)]
    (if (= (count conditions) 0)
      lists
      (let [where (nth conditions 0)]
        (filter #(is-between (nth % (get where :col-index)) (get where :value)) lists)))))

(defn get-operator [words] (nth words (+ (.indexOf words "where") 2)))

(defn get-where-col-index [words]
  (let [where-index (.indexOf words "where")
        col         (nth words (inc where-index))]
    (.indexOf (get-file-cols (get-filename words)) col)))

(defn get-value [words]
  (let [operatorIndex (+ (.indexOf words "where") 2)]
    [(Integer. (nth words (+ operatorIndex 1)))
     (Integer. (nth words (+ operatorIndex 3)))]))

; where format: {
;    :col-index: 0
;    :operator: "between"
;    :value: [2, 3]
;}
(defn parse-where [words]
  (let [operator  (get-operator words)
        col-index (get-where-col-index words)]
    (if (not= operator "between")
      (println "Operator" operator "is not supported yet :("))
    (if (= col-index -1)
      (println "Column you specified in where clause doesn't exist"))
    {:operator operator :value (get-value words) :col-index col-index}))

(defn get-conditions [words]
  (if (not= (.indexOf words "where") -1) [(parse-where words)] []))

(defn select [filename cols where isDistinct]
  (let [file-cols   (get-file-cols filename)
        indexes     (map #(.indexOf file-cols %) cols)
        wrong-col-i (.indexOf indexes -1)]
    (if (not= wrong-col-i -1)
      (println "Col" (nth cols wrong-col-i) "doesn't exist")
      (let [lists (map #(get-cols-from-row % indexes) (filter-where filename where))]
        (if (or (nil? isDistinct) (false? isDistinct))
          lists
          (filter-distinct lists))))))

(defn parse-line [line]
  (let [words (string/split line #"\s+")]
    (if (not= (string/lower-case (nth words 0)) "select")
      (println "wrong order")
      (let [filename   (get-filename words)
            cols       (get-query-cols words)
            isDistinct (not= (.indexOf words "distinct") -1)
            where      (get-conditions words)
            results    (select filename cols where isDistinct)]
        (output-by-line filename results cols)
        (if (= (count results) 0) (println "No rows match your query"))
        (println "Enter next command: ")))))

(defn execute-command [line] (if (not= line "exit") (parse-line line)))

(defn repl []
  (let [line (string/trim (read-line))]
    (if (command-exists (string/lower-case line))
      (execute-command line)
      (println "Wrong command!"))
    (if (not= line "exit") (repl))))

(println "Files available: " (string/join ", " files))
(println "Enter command:")

(repl)
