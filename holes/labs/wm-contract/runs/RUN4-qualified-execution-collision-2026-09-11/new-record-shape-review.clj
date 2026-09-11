(require '[futon3c.wm.run4-terminal-evidence-test :as t] '[futon3c.wm.run4-terminal-evidence :as e] '[futon2.aif.full-loop-cohort :as c] '[futon2.aif.c-fold-config :as d] '[clojure.edn :as edn])
(#'t/fixture
 (fn [{:keys [roots run-file projection-file binding-file projection binding execution-cohort]}]
  (let [authority (c/execution-authority execution-cohort)
        record (assoc (edn/read-string (slurp run-file))
                      :runner-execution/identity (c/execution-identity authority "attempt-001")
                      :runner-execution/provenance (c/execution-provenance authority "attempt-001"))]
   (#'t/write! run-file record)
   (let [sha (d/sha256 (slurp run-file))
         p (assoc-in projection [:source :run-record-sha256] sha)]
    (#'t/write! projection-file p)
    (#'t/write! binding-file (assoc binding :run4/terminal-projection
        (assoc (:run4/terminal-projection binding) :sha256 (d/sha256 (pr-str p)) :source-sha256 sha)))
    (prn (try {:bundle? (boolean (e/read-terminal-evidence-bundle roots t/request t/started))}
          (catch clojure.lang.ExceptionInfo ex (ex-data ex))))))))
