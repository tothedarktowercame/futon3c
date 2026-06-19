#!/usr/bin/env bb
(require '[futon3c.flight.pretty-print :as pretty])

(defn usage []
  (binding [*out* *err*]
    (println "Usage:")
    (println "  scripts/flight_pretty_print.clj --file data/repl-traces/<run>.flight.edn")
    (println "  scripts/flight_pretty_print.clj --run-id live-...")
    (println "  scripts/flight_pretty_print.clj --latest")
    (println)
    (println "Options:")
    (println "  --mapping data/flight-pretty-print-mapping.edn"))
  (System/exit 2))

(defn parse-args [args]
  (loop [m {:mapping pretty/default-mapping-path}
         xs args]
    (case (first xs)
      nil m
      "--file" (recur (assoc m :file (second xs)) (nnext xs))
      "--run-id" (recur (assoc m :run-id (second xs)) (nnext xs))
      "--latest" (recur (assoc m :latest? true) (next xs))
      "--mapping" (recur (assoc m :mapping (second xs)) (nnext xs))
      "-h" (usage)
      "--help" (usage)
      (usage))))

(defn selected-file [{:keys [file run-id latest?]}]
  (cond
    file file
    run-id (pretty/flight-path-for-run-id run-id)
    latest? (pretty/latest-flight-path)
    :else nil))

(defn -main [& args]
  (let [opts (parse-args args)
        path (selected-file opts)]
    (when-not path
      (usage))
    (print (pretty/render-file (:mapping opts) path))))

(apply -main *command-line-args*)
