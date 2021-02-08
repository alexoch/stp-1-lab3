(:require '[clojure.pprint]
          '[clojure.string :as str])

(load-file "utils.clj")

(defn contains-col-func? [col]
  (str/includes? col "("))

(defn get-col-func-name [word]
  (subs word 0 (.indexOf word "(")))

(defn get-colname [word]
  (subs word (inc (.indexOf word "(")) (.indexOf word ")")))

(defn get-col [col]
  (let [trimmed (trim-comma col)]
    (if (contains-col-func? trimmed)
      {:name trimmed :col (get-colname trimmed) :funcname (get-col-func-name trimmed)}
      {:col trimmed :name trimmed})))

; --- Order

; Order format: { :col string  :order "asc" | "desc" }
(defn get-order-by [words]
  (let [order-index (.indexOf words "order")]
    (if (= order-index -1)
      nil
      {:col   (nth words (+ order-index 2))
       :order (if (in? words "desc") "desc" "asc")})))

; --- Where

(defn get-value [words]
  [(Integer. (nth words 2))
   (Integer. (nth words 4))])

; conditions format: {:operator 'or' | 'and'  :conditions [condition | where] }
; single condition format: {:col str :operator between  :value: [2, 3]}
(defn parse-single-condition [words]
  {:operator (nth words 1)
   :value    (get-value words)
   :col      (if (contains-col-func? (first words)) (get-col (first words)) (first words))})

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

(defn get-where-conditions [words]
  (get-conditions-step (subvec words (inc (.indexOf words "where")))))

(defn get-having-conditions [words]
  (get-conditions-step (subvec words (inc (.indexOf words "having")))))

; --- Select

(defn get-case-condition [words]
  (get-conditions-step words))

(defn get-case-value [words & colname]
  (if (= (subs (first words) 0 1) "'")
    ; string value
    (let [wordsStr (reduce (fn [coll word] (str coll " " word)) words)]
      (subs wordsStr 1 (dec (count wordsStr))))
    ;number value
    (first words)))

(defn get-case-whens [words]
  (let [when-words      (subvec words (.indexOf words "when")
                                (if (in? words "else") (.indexOf words "else") (.indexOf words "end")))
        whens           (split-by-indexes when-words (indexes-of when-words "when"))]
    ; mapping arrays of words like [mp_id between 1 and 2 then 3]
    (map
     (fn [when]
       {:condition (get-case-condition (subvec when 0 (.indexOf when "then")))
        :value     (get-case-value (subvec when (inc (.indexOf when "then"))))})
     whens)))

(defn get-case-col [words]
  {:col  (nth words 2)
   :name (last words)
   :when (get-case-whens words)
   :else (if (in? words "else")
           (get-case-value
            (subvec words (inc (.indexOf words "else")) (.indexOf words "end"))))})

; Col format {:col :name} | {:col :name :funcname} |
; {:col :name :when {:condition {:operator :value} :value string/number}[] :else? string/number }
(defn get-query-cols [words]
  (let [cols-start  (if (= (nth words 1) "distinct") 2 1)
        col-words   (subvec words cols-start (.indexOf words "from"))
        case-words  (indexes-of col-words "case")
        end-words   (map #(+ % 2) (indexes-of col-words "end"))
        case-ranges (map-indexed (fn [i item] [item (nth end-words i)]) case-words)]
    (if (= (count case-words) 0)
      (map get-col col-words)
      (keep-indexed
       (fn [i item]
         (if (= item "case")
           (get-case-col (subvec col-words i (inc (nth end-words (.indexOf case-words i)))))
           (if (not (some #(and (>= i (first %)) (<= i (last %))) case-ranges))
             (get-col item))))
       col-words))))

; --- Group by

(defn get-group-cols [words]
  (let [start-index (+ (.indexOf words "group") 2)
        last-col    (first (filter #(not (str/includes? % ",")) (subvec words start-index)))
        end-index   (.indexOf words last-col)]
    (map trim-comma (subvec words start-index (inc end-index)))))

; --- Join

(defn get-tablename [word] (subs word 0 (index-of-last word ".")))
(defn get-join-colname [word] (subs word (inc (index-of-last word "."))))

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
    [{:table (get-tablename left-table) :col (get-join-colname left-table)}
     {:table (get-tablename right-table) :col (get-join-colname right-table)}]))

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
; having
; group by
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

          (if (in? words "having")
            {:command "having"
             :options (get-having-conditions words)
             :queries (parse-line (subvec words 0 (.indexOf words "having")))}

            (if (in? words "group")
              {:command "group"
               :options {:cols (get-group-cols words)}
               :queries (parse-line (subvec words 0 (.indexOf words "group")))}

              (if (in? words "where")
                {:command "where"
                 :options (get-where-conditions words)
                 :queries (parse-line (subvec words 0 (.indexOf words "where")))}

                (if (in? words "join")
                  (let [type (get-join-type words)]
                    {:command "join"
                     :options {:type type :on (get-join-on words)}
                     :queries [(parse-line (get-left-table words type)) (parse-line (get-right-table words))]})

                  {:command "get"
                   :from    (if (> (count words) 0)
                              (if (in? words "from") (nth words (inc (.indexOf words "from"))) (first words))
                              nil)})))))))))
