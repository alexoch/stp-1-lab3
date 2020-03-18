(ns myconj.core
  (:gen-class)
  (:use [clojure.data.csv])
  (:require [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn readTSV [name]                                        ;парсит tsv file
  (map #(str/split % #"\t")
       (str/split (slurp name) #"\r\n")))

(defn readCSV [name]                                        ; парсит csv filr
  (with-open [reader (io/reader name)]
    (doall
      (csv/read-csv reader))))

(defn data->maps [head & lines]                             ;крнвертирует данніе в колекцію map ключ значение
  (map #(zipmap (map keyword head) %1) lines))

(defn makeTableTSV [name]                                   ;создает табличку с tsv файла
  (apply data->maps (readTSV name)))

(defn makeTableCSV [name]                                   ;создает табличку с csv файла
  (apply data->maps (readCSV name)))

(defn printTable [table]                                    ;вивод таблички в консоль
  (loop [k 0]
    (when (< k (count (first table)))
      (print (format "%40s| " (name (nth (keys (first table)) k))))
      (recur (+ k 1))))
  (println)
  (loop [i 0]
    (when (< i (count table))
      (loop [j 0]
        (when (< j (count (nth table i)))
          (print (format "%40s| " (nth (vals (nth table i)) j)))
          (recur (+ j 1))))
      (println "")
      (recur (+ i 1)))))

(defn checkFormat [name]                                    ;прроверка формата файла
  (def formatFile (str/split name #"\."))
  (if (= (str (nth formatFile 1)) "csv") true false))

(defn -main
  [& args]
  (println "Input name of file: ")
  (def input (read-line))                                   ;запрос на ввод строки
  ;(def input "mp-assistants.csv")
  (if (checkFormat input)
    (printTable (makeTableCSV input))
    (printTable (makeTableTSV input))))