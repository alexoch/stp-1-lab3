(require
 '[clojure.java.io :as io]
 '[clojure.string :as str]
 '[clojure.set :as set])

(load-file "utils.clj")
(load-file "join.clj")

; Table format:
; { name: string, head: list, body: list[] | list[][], isGrouped?: boolean }

(defn table [old-table & body]
  {:name      (get old-table :name)
   :head      (get old-table :head)
   :isGrouped (get old-table :isGrouped)
   :body      (if (nil? body) (get old-table :body) body)})

; --- Get file lists

(defn get-type [filename] (nth (str/split filename #"\.") 1))
(defn get-delim [filename] (if (= (get-type filename) "csv") #"," #"\t"))

(defn get-file-lists [filename]
  (map #(str/split % (get-delim filename))
       (str/split-lines (slurp (str "files/" filename)))))

; --- Order

(defn order-func [order]
  (fn [col] (let [val (nth col order)] (if (= val "null") 0 (Integer. val)))))

(defn order-lists [{name :name head :head body :body} options]
  (let [col-index     (.indexOf head (get options :col))
        order-compare (order-func col-index)
        compare-func  (if (= (get options :order) "asc") < >)]
    {:name name
     :head head
     :body (sort-by order-compare compare-func body)}))

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

(defn select-cols [{name :name head :head body :body} options]
  (let [cols             (get options :cols)
        func-col-indexes (get-func-cols-indexes cols)
        indexes          (map #(.indexOf head %) cols)
        funcs-res        (map #(apply-col-func body %) cols)]
    {:name name
     :head cols
     :body (if (= (count func-col-indexes) (count cols))
             ; if all are col funcs, return one row
             [funcs-res]
             (map #(get-cols-from-row % indexes func-col-indexes funcs-res) body))}))

; --- Union

(defn is-union-valid [tables]
  (apply = (map #(count (get % :head)) tables)))

(defn get-union [tables options]
  (if (is-union-valid tables)
    {:name (get (first tables) :name)
     :head (get (first tables) :head)
     :body (distinct (apply concat (map #(get % :body) tables)))}
    (println "Can't perform union on the lists given ¯\\_(ツ)_/¯")))

; --- Distinct

(defn filter-distinct [{name :name head :head body :body}]
  {:name name
   :head head
   :body (distinct body)})

; --- Where

(defn is-between [val range]
  (if
    (= val "null")
    false
    (let [int-val  (Integer. val)]
      (and (>= int-val (first range)) (<= int-val (nth range 1))))))

(declare filter-where)

(defn map-and-where [and-where table]
  {:name (get table :name)
   :head (get table :head)
   :body (reduce set/intersection
                 (map #(set (get (filter-where table %) :body)) (get and-where :conditions)))})

(defn map-or-where [or-where table]
  {:name (get table :name)
   :head (get table :head)
   :body (set
          (reduce into (map #(get (filter-where table %) :body) (get or-where :conditions))))})

(defn execute-filter [condition {name :name head :head body :body}]
  (let [col-index (.indexOf head (get condition :col))
        range     (get condition :value)]
    {:name name
     :head head
     :body (filter #(is-between (nth % col-index) range) body)}))

(defn filter-where [table where]
  (if (nil? (get where :conditions))
    (execute-filter where table)
    (if (= (get where :operator) "or")
      (map-or-where where table)
      (map-and-where where table))))

; --- Execute

(declare execute-command-step)

(defn execute-basic [command]
  (if (nil? (get command :from))
    nil
    (let [filename (get command :from)
          lists    (get-file-lists filename)]
      {:name filename :head (first lists) :body (rest lists)})))

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

(defn execute-complex [command]
  (let [options      (get command :options)
        func         (complex-func command)
        prev-results (if (is-composite-queries command)
                       (map #(execute-command-step %) (get command :queries))
                       (execute-command-step (get command :queries)))]
    (println "Executing" (get command :command))
    (if (= (get command :command) "distinct")
      (func prev-results)
      (func prev-results options))))

(defn execute-command-step [command]
  (if (nil? (get command :queries))
    (execute-basic command)
    (execute-complex command)))

(defn execute-command [command]
  (execute-command-step command))
