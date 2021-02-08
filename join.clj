(load-file "utils.clj")

(defn inner-join-tables [[{table1 :rows col-index1 :col-index} {table2 :rows col-index2 :col-index}]]
  (apply concat
         (keep
          (fn [table1-row]
            (let [table1-val       (nth table1-row col-index1)
                  table2-join-rows (filter #(= table1-val (nth % col-index2)) table2)]
              (if (not= (count table2-join-rows) 0)
                (map #(concat table1-row %) table2-join-rows))))
          table1)))

(defn make-empty-row [table] (repeat (count (first table)) "null"))

(defn full-join-left-table [[{table1 :rows col-index1 :col-index} {table2 :rows col-index2 :col-index}]]
  (let [empty-row (make-empty-row table2)]
    (apply concat
           (map
            (fn [table1-row]
              (let [table1-val       (nth table1-row col-index1)
                    table2-join-rows (filter #(= table1-val (nth % col-index2)) table2)
                    table2-rows      (if (= (count table2-join-rows) 0) [empty-row] table2-join-rows)]
                (map #(concat table1-row %) table2-rows)))
            table1))))

(defn empty-join-right-table [[{table1 :rows col-index1 :col-index} {table2 :rows col-index2 :col-index}]]
  (let [empty-row (make-empty-row table1)]
    (keep
     (fn [table2-row]
       (let [table2-val      (nth table2-row col-index2)
             has-match       (does-match #(= table2-val (nth % col-index1)) table1)]
         (if has-match nil (concat empty-row table2-row))))
     table2)))

(defn full-outer-join-tables [options]
  (concat (full-join-left-table options)
          (empty-join-right-table options)))

(defn get-join-side [table options]
  {:rows      (get table :body)
   :col-index (.indexOf (get table :head) (get options :col))})

(defn get-full-header [table]
  (map #(str (get table :name) "." %) (get table :head)))

(defn get-join-header [tables]
  (concat (get-full-header (first tables)) (get-full-header (nth tables 1))))

; options format
; on-options format [{:rows :col-index} {:rows :col-index}]
(defn join-tables [tables options]
  (let [on          (get options :on)
        left        (first on)
        right       (last on)
        on-options  [(get-join-side (first tables) left) (get-join-side (last tables) right)]
        header      (get-join-header tables)]
    {:name (get (first tables) :name)
     :head header
     :body (rest
            (case (get options :type)
                  "inner"      (inner-join-tables on-options)
                  "full outer" (full-outer-join-tables on-options)))}))
