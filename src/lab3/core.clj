(ns lab3.core
  (:gen-class)
  (:use [clojure.data.csv])
  (:require [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn getDataFromTSV [name]
  (map #(str/split % #"\t")
       (str/split (slurp name) #"\r\n"))
            (println (slurp name))
  )
  

(defn getDataFromCSV [name]
  (with-open [reader (io/reader name)]
      (doall
          (csv/read-csv reader))
      )
  )

(defn fromDataToMap [head & lines]
  (map #(zipmap
          (map keyword head) %1) lines)
  )

(defn makeTableTSV [name]
  (apply
      fromDataToMap
          (getDataFromTSV name)
      )
  )

(defn makeTableCSV [name]
  (apply
      fromDataToMap
          (getDataFromCSV name)
      )
  )

(defn createFormatTable [table]
  (loop [k 0]
      (when (< k (count (first table)))
          (print
              (format "|%5s|" (name (nth (keys (first table)) k))))
                  (recur (+ k 1))))
                      (println)
                          (loop [i 0]
                              (when (< i (count table))
                                  (loop [j 0]
                                      (when (< j (count (nth table i)))
                                          (print (format "|%5s|" (nth (vals (nth table i)) j)))
                                              (recur (+ j 1))))
                                                  (println "")
                                                      (recur (+ i 1)))))

(defn checkInputFile [name]
    (def formatFile (str/split name #"\."))
        (if (= (str (nth formatFile 1)) "csv") true false))

(defn -main
  [& args]
  (println "Enter name of file: ")
  (def input (read-line))
  (if (checkInputFile input)
          (createFormatTable (makeTableCSV input))
              (createFormatTable (makeTableTSV input)
                                 )
          )
  )
