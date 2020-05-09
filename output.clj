(def MAX-VAL-LEN 25)

(defn col-to-str [col]
  (str "| " col (apply str (repeat (- MAX-VAL-LEN (count col)) " ")) " "))

(defn get-long-value [col]
  (str (subs col 0 (- MAX-VAL-LEN 3)) "..."))

(defn output-by-line [lists]
  (doseq [list lists]
    (println
     (apply str
            (map #(col-to-str (if (< (count %) MAX-VAL-LEN) % (get-long-value %))) list)))))
