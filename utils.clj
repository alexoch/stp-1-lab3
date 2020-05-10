(require
 '[clojure.java.io :as io]
 '[clojure.string :as str])

(defn is-between [val range]
  (if
    (= val "null")
    false
    (let [int-val  (Integer. val)]
      (and (>= int-val (first range)) (<= int-val (last range))))))

(defn flatten-once [vec] (reduce into vec))

(defn trim-comma [str]
  (if (.endsWith str ",") (.substring str 0 (dec (count str))) str))

(defn get-table-head [lists] (first lists))

(defn get-col-index [col lists]
  (.indexOf (get-table-head lists) col))

(defn get-filename [words]
  (let [from-index (.indexOf words "from")]
    (nth words (inc from-index))))

(defn get-words [line] (str/split line #"\s+"))

(defn find-first  [f coll] (first (filter f coll)))
(defn index-of-first [f coll] (first (keep-indexed #(when (f %2) %) coll)))
(defn does-match [f coll] (not (nil? (some f coll))))

(defn split-by-indexes [arr indexes]
  (if (= (count indexes) 0)
    [arr]
    (let [res (map-indexed
               #(let [start (if (= % 0) 0 (inc (nth indexes (dec %))))
                      end   %2]
                 (subvec arr start end))
               indexes)]
      (filter #(> (count %) 0) (concat res [(subvec arr (inc (last indexes)))])))))

(defn delete-by-index [vec index]
  (into [] (concat (subvec vec 0 index) (subvec vec (inc index)))))

(defn in? [coll elm] (some #(= elm %) coll))

(defn index-of-last [str substr]
  (dec (- (count str) (.indexOf (str/reverse str) substr))))

(defn indexes-of [vec elem] (keep-indexed #(if (= %2 elem) %) vec))
