(:require '[clojure.pprint])

(load-file "utils.clj")

; Order format: { :col string  :order "asc" | "desc" }
(defn get-order-by [words]
  (let [order-index (.indexOf words "order")]
    (if (= order-index -1)
      nil
      {:col   (nth words (+ order-index 2))
       :order (if (in? words "desc") "desc" "asc")})))

; --- Select

(defn trim-col [col] (trim-comma col))

(defn get-query-cols [words]
  (let [cols-start (if (= (nth words 1) "distinct") 2 1)]
    (map trim-col (subvec words cols-start (.indexOf words "from")))))

; --- Where

(defn get-value [words]
  [(Integer. (nth words 2))
   (Integer. (nth words 4))])

; where format: {:operator 'or' | 'and'  :conditions [condition | where] }
; condition format: {:col str :operator: between  :value: [2, 3]}
(defn parse-single-condition [words]
  {:operator (nth words 1) :value (get-value words) :col (first words)})

(defn is-and-operator [word index words]
  (and (= word "and")
       ; between a and b
       (or (< index 2) (not= (nth words (- index 2)) "between"))))

(defn or-indexes [words]
  (keep-indexed #(if (= %2 "or") %) words))

(defn and-indexes [words]
  (keep-indexed #(if (is-and-operator %2 % words) %) words))

(defn split-by-and [arr] (split-by-indexes arr (and-indexes arr)))
(defn split-by-or [arr] (split-by-indexes arr (or-indexes arr)))

(defn contain-or? [words]
  (in? words "or"))
(defn contain-and? [words] (not= (count (and-indexes words)) 0))

(defn get-conditions-step [words]
  (if  (contain-or? words)
    {:operator   "or"
     :conditions (map get-conditions-step (split-by-or words))}
    (if (contain-and? words)
      {:operator "and" :conditions (map get-conditions-step (split-by-and words))}
      (parse-single-condition words))))

(defn get-conditions [words]
  (get-conditions-step (subvec words (inc (.indexOf words "where")))))

; --- Join

(defn get-tablename [word] (subs word 0 (index-of-last word ".")))
(defn get-colname [word] (subs word (inc (index-of-last word "."))))

(defn get-join-type [words]
  (let [join-index (.indexOf words "join")]
    (if  (and (> join-index 1)
              (= (nth words (- join-index 1)) "outer")
              (= (nth words (- join-index 2)) "full"))
      "full outer"
      "inner")))

(defn get-join-on [words]
  (let [on-index    (.indexOf words "on")
        left-table  (nth words (inc on-index))
        right-table (nth words (+ on-index 3))]
    [{:table (get-tablename left-table) :col (get-colname left-table)}
     {:table (get-tablename right-table) :col (get-colname right-table)}]))

(defn get-left-table [words join-type]
  (subvec words 0
          (if (= join-type "full outer")
            (.indexOf words "full")
            (max (.indexOf words "inner") (.indexOf words "join")))))

(defn get-right-table [words]
  (subvec words (inc (.indexOf words "join")) (.indexOf words "on")))

; ---

; Priorities:
;
; order
; union
; distinct
; select
; where
; join

; Foramt: {:command :queries :options} / {:command :from}
(defn parse-line [words]
  (if (in? words "order")
    {:command "order"
     :options (get-order-by words)
     :queries (parse-line (subvec words 0 (.indexOf words "order")))}

    (if (in? words "union")
      {:command "union"
       :queries (map parse-line (split-by-indexes words (indexes-of words "union")))}

      (if (in? words "distinct")
        {:command "distinct"
         :queries (parse-line (delete-by-index words (.indexOf words "distinct")))}

        (if (in? words "select")
          {:command "select"
           :options {:cols (get-query-cols words)}
           :queries (parse-line (subvec words (.indexOf words "from")))}

          (if (in? words "where")
            {:command "where"
             :options (get-conditions words)
             :queries (parse-line (subvec words 0 (.indexOf words "where")))}

            (if (in? words "join")
              (let [type (get-join-type words)]
                {:command "join"
                 :options {:type type :on (get-join-on words)}
                 :queries [(parse-line (get-left-table words type)) (parse-line (get-right-table words))]})

              {:command "get"
               :from    (if (> (count words) 0)
                          (if (in? words "from") (nth words (inc (.indexOf words "from"))) (first words))
                          nil)})))))))
