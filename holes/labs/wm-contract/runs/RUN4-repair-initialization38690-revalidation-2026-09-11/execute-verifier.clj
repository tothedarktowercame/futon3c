(require '[cheshire.core :as json] '[clojure.pprint :as pp]
 '[futon2.aif.c-fold-config :as digest]
 '[futon3c.wm.run4-historical-verification :as verifier])
(def base "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-initialization38690-revalidation-2026-09-11")
(let [job-text (slurp (str base "/independent-review-job.json"))
 response (json/parse-string job-text true) job (:job response)
 jid "invoke-1789152780470-20240-9bf4213d"
 finding "/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-initialization-38690d22-879c-47ab-a72a-6aaf2cce85fa-initialization-failed.edn"
 opts {:finding-root "/home/joe/code/futon2/data/wm-repair-obligations/findings"
 :qualification-root (str base "/offline-evidence")
 :qualification-source-root "/home/joe/code"
 :output-root (str base "/offline-verification")
 :source-repo "/home/joe/code/futon2"
 :finding-path finding :finding-sha256 (digest/sha256 (slurp finding))
 :qualification-path (str base "/offline-evidence/repair-initialization38690-revalidation-20260911-v1.qualification.edn")
 :qualification-sha256 "05b65e2b339ca039452c2d47d70c2de459da4c286325c50e5b3229623324f921"
 :expected-check-ids [:distinct-root-runner-regression]
 :first-commit "8788443d7cf0c806261933e2c68009d84f57819d"
 :last-commit "810be2a9a19d70b054d9ef7ceb43a2349b7a923d"
 :source-head "810be2a9a19d70b054d9ef7ceb43a2349b7a923d"
 :verification-id "repair-initialization38690-revalidation-20260911-v1"
 :author "codex-17" :reviewer "zai-1" :review-job-id jid}]
 (assert (true? (:ok response)))
 (assert (= "done" (:state job))) (assert (= jid (:job-id job)))
 (assert (= "zai-1" (:agent-id job)))
 (spit (str base "/verifier-input.edn") (with-out-str (pp/pprint {:options opts :review-sha256 (digest/sha256 job-text)})))
 (let [r (verifier/admit! (assoc opts :review-job-reader (fn [requested] (assert (= jid requested)) job)))]
  (prn (select-keys r [:schema :verification-id :repair-id :state :repair-resolved? :actors :review]))))
(shutdown-agents)
