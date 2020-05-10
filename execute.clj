(require
 '[clojure.string :as str]
 '[clojure.set :as set])

(load-file "utils.clj")
(load-file "join.clj")

; Table format:
; { name: string, head: list, body: list[] | list[][], is-grouped?: boolean, grouped-by?: colname[] }

(defn table [old-table & body]
  {:name       (get old-table :name)
   :head       (get old-table :head)
   :is-grouped (get old-table :is-grouped)
   :body       (if (nil? body) (get old-table :body) body)})

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

(defn average [numbers]
  (float (/ (reduce + numbers) (count numbers))))

(defn get-col-numeric [col]
  (map #(Integer. %) col))

(defn get-avg [col] (str (average (get-col-numeric col))))
(defn get-max [col] (str (reduce max (get-col-numeric col))))
(defn get-count [col] (str (count col)))

(defn get-col-func [func-col]
  (case (get func-col :funcname)
        "count"   get-count
        "avg"     get-avg
        "max"     get-max
        "default" (println "Col function is not supported")))

(defn apply-col-func [body {index :col-index func :func}]
  (func (map #(nth % index) body)))

(defn get-row-col [row index]
  (if (< (count row) (inc index)) nil (nth row index)))

(defn is-col-func? [col] (not (nil? (get col :funcname))))
(defn is-col-case? [col] (not (nil? (get col :when))))
(defn is-simple-col? [col] (not (or (is-col-func? col) (is-col-case? col))))

(defn get-func-cols-indexes [cols]
  (keep-indexed #(if (is-col-func? %2) %) cols))

(defn get-col-funcs [head cols]
  (keep
   #(if (is-col-func? %)
     {:func      (get-col-func %)
      :col-index (.indexOf head (get % :col))})
   cols))

; Example inputs:
; reducers: [(nth row 5) (nth row 0) nil (nth row 2) nil nil]            + wrapped in fn [row] ()
; func-col-indexes: [2 4 5]
(defn get-cols-from-row [row reducers funcs-res]
  (let [func-col-indexes (indexes-of reducers nil)]
    (map-indexed
      (fn [i reducer]
        (if (nil? reducer)
          (nth funcs-res (.indexOf func-col-indexes i))
          (reducer row)))
      reducers)))

(defn get-col-results [body
                       {reducers   :reducers
                        col-funcs  :col-funcs
                        is-grouped :is-grouped}]
  (let [funcs-res        (map #(apply-col-func body %) col-funcs)]
    (if (or is-grouped (= (count funcs-res) (count reducers)))
      ; return one row if is grouped or all cols are col funcs
      [(get-cols-from-row (first body) reducers funcs-res)]
      (map #(get-cols-from-row % reducers funcs-res) body))))

(defn get-row-reducers [cols head]
  (map
   (fn [col]
     (if (not (is-col-func? col))
       (let [col-index (.indexOf head (get col :col))]
         (if (is-simple-col? col)
           (fn [row] (nth row col-index))
           (fn [row]
             (let [true-when-index (index-of-first
                                    (fn [{cond :condition}] (is-between (nth row col-index) (get cond :value)))
                                    (get col :when))]
               (if (nil? true-when-index)
                 (get col :else)
                 (get (nth (get col :when) true-when-index) :value))))))))
   cols))

; Col format {:name string :col string} | {:name string :col string :funcname funcname} |
; {:name string :col string :when {: condition {:operator :value} :value string/number}[] :else? string/number }
(defn select-cols [{name :name head :head body :body is-grouped :is-grouped grouped-by :grouped-by}
                   {cols :cols}]
  (let [options            {:reducers   (get-row-reducers cols head)
                            :col-funcs  (get-col-funcs head cols)
                            :is-grouped is-grouped}
        ]
    {:name       name
     :head       (map #(get % :name) cols)
     :is-grouped false
     :body       (if is-grouped
                   (flatten-once (concat (map #(get-col-results % options) body)))
                   (get-col-results body options))}))

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

; --- Having

(defn group-having? [head body {val :value col :col}]
  (if(is-col-func? col)
    (let [func     (get-col-func col)
          index    (.indexOf head (get col :col))
          func-res (apply-col-func body {:col-index index :func func})]
      (is-between func-res val))))

(defn filter-having [{name :name head :head body :body is-grouped :is-grouped grouped-by :grouped-by}
                     options]
  {:name       name
   :head       head
   :is-grouped true
   :grouped-by grouped-by
   :body       (filter #(group-having? head % options) body)})

; --- Group

(defn group-func [indexes]
  (fn [row] (map #(nth row %) indexes)))

(defn group-table [{name :name head :head body :body} {cols :cols}]
  (let [col-indexes (map #(.indexOf head %) cols)
        func        (group-func col-indexes)]
    {:name       name
     :head       head
     :body       (vals (group-by func body))
     :is-grouped true
     :grouped-by cols}))

; --- Where

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
          (flatten-once
           (map #(get (filter-where table %) :body) (get or-where :conditions))))})

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
        "having"   filter-having
        "group"    group-table
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
