(require
 '[clojure.string :as string])

(load-file "parse.clj")
(load-file "execute.clj")
(load-file "output.clj")

(def files ["skl1.tsv", "skl2.csv", "posts.csv", "assistants.csv", "test.csv"])

(defn command-exists [line]
  (or (= line "exit")
      (and (string/includes? line "select") (string/includes? line "from"))))

(defn get-results [line]
  (if (not= line "exit")
    (output-by-line (execute-command (parse-line (get-words line)))))
  (if (not= line "exit") (println "Enter next command: ")))

(defn repl []
  (let [line (string/trim (read-line))]
    (if (command-exists (string/lower-case line))
      (get-results line)
      (println "Wrong command!"))
    (if (not= line "exit") (repl))))

(println "Files available: " (string/join ", " files))
(println "Enter command:")
(repl)
