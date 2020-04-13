(ns lab3.core
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn read-file
  [fname]
  (slurp fname))

(defn parse-by-line
  [text]
  (str/split text #"\n"))

(defn split-by-comma
  [row]
  (str/split row #"(,(?=\S)|:)"))

(defn create-hash-map
  [row first-line]
  (zipmap first-line row))

(defn create-list-of-lists-by-comma
  [lists]
  (map split-by-comma lists))

(defn is-csv
  [filename]
  (def format (second(str/split filename #"[.]")))
  (= format "csv"))

(defn csv
  [text]
  (def list-of-lists (create-list-of-lists-by-comma text))
  (def first-row (first list-of-lists))
  (def list-of-lists-without-first-row (drop 1 list-of-lists))
  (def result (map (fn [item] (create-hash-map item first-row)) list-of-lists-without-first-row))
  (pp/pprint result))

(defn split-by-tabs
  [row]
  (str/split row #"[\t]"))

(defn create-list-of-lists-by-tabs
  [lists]
  (map split-by-tabs lists))

(defn tsv
  [text]
  (def list-of-lists (create-list-of-lists-by-tabs text))
  (def first-row (first list-of-lists))
  (def list-of-lists-without-first-row (drop 1 list-of-lists))
  (def result (map (fn [item] (create-hash-map item first-row)) list-of-lists-without-first-row))
  (pp/pprint result))

(defn load
  [fileName]
  (def text (read-file fileName))
  (def parsed-text (parse-by-line text))
  (if (is-csv fileName) (csv parsed-text) (tsv parsed-text)))