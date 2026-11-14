;; M-repl-circus invariant report (read-only). Two invariants over the
;; watched mission/excursion/ticket/campaign documents plus the giraffe
;; reread requisition ledger:
;;   1. one document per id  (duplicate file names; duplicate futon1b
;;      :mission/doc|:excursion/doc|:ticket/doc external ids)
;;   2. a readable status    (watcher-classified status not :unknown/nil)
;; Writes nothing; opens fts5-evidence.db read-only (mode=ro).
;;
;;   clojure -M scripts/mission_doc_invariants.clj [--json] [--no-db]
;;          [--root PATH]... [--ledger PATH]...
;;
;; The logic namespace requires the watcher parsers, whose require chain
;; (mission-control-backend → agency.registry → blackboard) needs dev/ on
;; the classpath. Plain -M lacks it, so run:
;;   clojure -Sdeps '{:aliases {:mdi {:extra-paths ["dev"]}}}' \
;;     -M:mdi scripts/mission_doc_invariants.clj
;;
;; --root/--ledger override the default tables (repeatable) so the script
;; can be pointed at a single checkout.
(require '[futon3c.logic.mission-doc-invariants :as inv]
         '[futon3c.watcher.roots :as roots]
         '[cheshire.core :as json]
         '[clojure.java.io :as io])

(defn parse-args [args]
  (loop [m {:json false :db true :roots [] :ledgers []} [a & more] args]
    (if-not a m
      (case a
        "--json" (recur (assoc m :json true) more)
        "--no-db" (recur (assoc m :db false) more)
        "--root" (recur (update m :roots conj (first more)) (rest more))
        "--ledger" (recur (update m :ledgers conj (first more)) (rest more))
        (do (binding [*out* *err*] (println "unknown arg:" a))
            (System/exit 2))))))

(defn -main [& args]
  (let [{:keys [json db roots ledgers]} (parse-args args)
        watch-roots (if (seq roots)
                      (mapv (fn [p] {:path p :label (.getName (io/file p))}) roots)
                      roots/watch-roots)
        ledger-paths (if (seq ledgers) ledgers inv/default-ledger-paths)
        docs (inv/scan-docs watch-roots ledger-paths)
        v (inv/violations {:docs docs
                           :db-path (when db inv/default-db-path)})]
    (if json
      (println (json/generate-string
                (cond-> v
                  true (assoc :doc-count (count docs)))))
      (do
        (println "scanned" (count docs) "documents across"
                 (count watch-roots) "roots +" (count ledger-paths) "ledgers")
        (doseq [line (inv/summarize v)] (println line))
        (doseq [{:keys [external-id paths]} (get-in v [:one-document-per-id :files])]
          (println "  dup file" external-id)
          (doseq [p paths] (println "    " p)))
        (doseq [{:keys [external-id entity-ids]} (get-in v [:one-document-per-id :futon1b-entities])]
          (println "  dup entity" external-id)
          (doseq [i entity-ids] (println "    " i)))
        (doseq [{:keys [path kind raw-status]} (:readable-status v)]
          (println "  unknown status" (name kind) path
                   "| raw:" (pr-str raw-status)))))
    (shutdown-agents)))
(apply -main *command-line-args*)
