(require
 '[clojure.java.io :as io]
 '[clojure.string :as str]
 '[clojure.set :as set])

(load-file "utils.clj")
(load-file "join.clj")

; Table format:
; { tablename: string, head: list, body: list[] | list[][], isGrouped: boolean }

(defn get-file-head [filename]
  (str/split
   (with-open [rdr (io/reader (str "files/" filename))]
     (nth (line-seq rdr) 0))
   (get-delim filename)))

(defn and-table-head [lists rows]
  (concat [(get-table-head lists)] rows))

; --- Order

(defn order-func [order]
  (fn [col] (let [val (nth col order)] (if (= val "null") 0 (Integer. val)))))

(defn order-lists [lists options]
  (let [col-index     (get-col-index (get options :col) lists)
        order-compare (order-func col-index)
        compare-func  (if (= (get options :order) "asc") < >)]
    (and-table-head lists
                    (sort-by order-compare compare-func (rest lists)))))

; --- Select

(defn contains-col-func? [col]
  (str/includes? col "("))

(defn get-col-func [word]
  (subs word 0 (.indexOf word "(")))

(defn get-col [word] (subs word (inc (.indexOf word "(")) (.indexOf word ")")))

(defn average [numbers]
  (float (/ (reduce + numbers) (count numbers))))

(defn get-col-numeric [col]
  (map #(Integer. (first %)) col))

(defn get-avg [col] (str (average (get-col-numeric col))))
(defn get-max [col] (str (reduce max (get-col-numeric col))))
(defn get-count [col] (str (count col)))

(defn apply-col-func [rows func-col]
  (let [func      (get-col-func func-col)
        col-name  (get-col func-col)
        col-index (get-col-index col-name rows)
        col       (filter #(nth % col-index) (rest rows))]
    (case func
          "count"   (get-count col)
          "avg"     (get-avg col)
          "max"     (get-max col)
          "default" (println "Col function is not supported"))))

(defn get-row-col [row index]
  (if (< (count row) (inc index)) nil (nth row index)))

(defn get-cols-from-row [row indexes func-indexes func-res]
  (map-indexed
   #(if (in? func-indexes %)
     (nth func-res (.indexOf func-indexes %))
     (get-row-col row %2))
   indexes))

(defn get-func-cols-indexes [cols]
  (keep-indexed #(if (contains-col-func? %2) %) cols))

(defn select-cols [lists options]
  (let [cols             (get options :cols)
        head             (get-table-head lists)
        func-col-indexes (get-func-cols-indexes cols)
        indexes          (map #(.indexOf head %) cols)
        funcs-res        (map #(apply-col-func lists %) cols)]
    (concat [cols]
            (if (= (count func-col-indexes) (count cols))
              ; if all are col funcs, return one row
              [funcs-res]
              (map #(get-cols-from-row % indexes func-col-indexes funcs-res) (rest lists))))))

; --- Union

(defn is-union-valid [lists]
  (apply = (map #(count (get-table-head %)) lists)))

(defn get-union [lists options]
  (if (is-union-valid lists)
    (concat [(get-table-head (first lists))] (distinct (apply concat (map rest lists))))
    (println "Can't perform union on the lists given ¯\\_(ツ)_/¯")))

; --- Distinct

(defn filter-distinct [lists] (distinct lists))

; --- Where

(defn is-between [val range]
  (if
    (= val "null")
    false
    (let [int-val  (Integer. val)]
      (and (>= int-val (first range)) (<= int-val (nth range 1))))))

(declare filter-where)

(defn map-and-where [and-where lists]
  (and-table-head lists
                  (reduce set/intersection
                          (map #(set (rest (filter-where lists %))) (get and-where :conditions)))))

(defn map-or-where [or-where lists]
  (and-table-head lists
                  (set
                   (reduce into (map #(rest (filter-where lists %)) (get or-where :conditions))))))

(defn execute-filter [condition lists]
  (let [col-index (get-col-index (get condition :col) lists)
        range     (get condition :value)]
    (concat [(get-table-head lists)]
            (filter #(is-between (nth % col-index) range) (rest lists)))))

(defn filter-where [lists where]
  (if (nil? (get where :conditions))
    (execute-filter where lists)
    (if (= (get where :operator) "or")
      (map-or-where where lists)
      (map-and-where where lists))))

; --- Execute

(declare execute-command-step)

(defn execute-basic [command]
  (if (nil? (get command :from))
    nil
    (get-file-lists (get command :from))
;    (let [filename (get command :from)
;          lists    (get-file-lists filename)]
;      {:name filename :head (first lists) :body (rest lists)})

    ))

(defn complex-func [command]
  (case (get command :command)
        "order"    order-lists
        "select"   select-cols
        "union"    get-union
        "distinct" filter-distinct
        "where"    filter-where
        "join"     join-tables))

(defn is-composite-queries [command]
  (or (= (get command :command) "union") (= (get command :command) "join")))

(defn execute-complex [command lists]
  (let [options   (get command :options)
        func      (complex-func command)
        sub-lists (if (is-composite-queries command)
                    (map #(execute-command-step % lists) (get command :queries))
                    (execute-command-step (get command :queries) lists))]
    (println "Executing" (get command :command))
    (if (= (get command :command) "distinct")
      (func sub-lists)
      (func sub-lists options))))

(defn execute-command-step [command lists]
  (if (nil? (get command :queries))
    (execute-basic command)
    (execute-complex command lists)))

(defn execute-command [command]
  (execute-command-step command nil))
