(def MAX-VAL-LEN 25)

(defn col-to-str [col]
  (str "| " (if (nil? col) "nil" col) (apply str (repeat (- MAX-VAL-LEN (count col)) " ")) " "))

(defn get-long-value [col]
  (str (subs col 0 (- MAX-VAL-LEN 3)) "..."))

(defn output-row [row]
  (println
   (apply str
          (map #(col-to-str (if (< (count %) MAX-VAL-LEN) % (get-long-value %))) row))))

(defn output-by-line [table]
  (output-row (get table :head))
  (doseq [row (get table :body)] (output-row row)))
